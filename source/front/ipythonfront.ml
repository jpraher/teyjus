(* -----------------------------------------------------------------------------
 * Copyright (C) 2012 Jakob Praher
 *
 * Distributed under the terms of the BSD License. The full license is in
 * the file COPYING, distributed as part of this software.
 * -----------------------------------------------------------------------------
 *)

open Sys
open Unix
open Kernel
open Lexing
open Lpyacc

(* type MyHandler = Kernel.HandlerType; *)

external link : string -> int -> int = "FRONT_link"

module type TeyjusHandler = sig
  type ctx_t 
  val execute_request : Kernel.ip_shell -> ctx_t -> Kernel.execute_request_t -> Kernel.execute_response_t
end


let extract_signature = function
  | (Preabsyn.Module (name,gconsts,lconsts,cconsts,useonlyconsts,exportdefconsts,
             fixities, 
             gkinds, lkinds, 
             typeabbrevs, 
             clauses,
             accummods,accumsigs,usedsigs,impmods
            )) ->
     (Preabsyn.Signature (name,gconsts,useonlyconsts,exportdefconsts,
                 gkinds,
                 typeabbrevs,
                 fixities,
                 accumsigs,
                 usedsigs))

  | s -> s


let compile parse lexbuf filename =
  let () = Lplex.setFileName lexbuf filename in
  let result = parse Lplex.initial lexbuf in
  result

let suffix str len = (String.sub str len ((String.length str) - len))
  
exception CompileError of string
  
let raise_on_error descr = if !Errormsg.anyErrors then 
    begin
      prerr_endline ("Errormsg.anyErrors: " ^ descr);
      raise (CompileError descr)
    end

let create_temp_dir basedir prefix suffix  =
  let counter = ref 0 in
  let failed_create num dirname =
    let _ = dirname := (Filename.concat basedir  (prefix ^ (string_of_int num) ^ suffix)) in
    try 
      Unix.mkdir !dirname 0o750;
      false
    with Unix.Unix_error (EEXIST,_,_) -> true
  in
  let dir = ref "" in
  while failed_create !counter dir
  do
    counter := !counter + 1
  done;
  !dir

let create_temp_env m = create_temp_dir Filename.temp_dir_name "teyjus" m
  
let get_code : Yojson.Basic.json -> string = function obj -> Yojson.Basic.Util.to_string (Yojson.Basic.Util.member "code" obj)

let get_string_member mem  = function obj -> Yojson.Basic.Util.to_string (Yojson.Basic.Util.member mem  obj ) 

let get_input obj default = 
  try
    get_string_member "input" obj
  with 
    | e -> default

let get_module obj default = 
  try
    get_string_member "module" obj
  with 
    | e -> default


let handle_compile_program shell ctx code m =
  try
    let _ = Errormsg.reset () in 
    let modresult = compile Lpyacc.parseModule (Lexing.from_string code) (m ^ ".mod") in
    let _ = raise_on_error "Parse failed" in
    let sigresult = extract_signature modresult in 
    let (absyn, sigabsyn) = Translate.translate modresult sigresult in
    let _ = raise_on_error  "Construct absyn module failed" in
    (* Get the list of clauses and new clauses. *)
    let (absyn, clauses, newclauses, closeddefs) = 
      Clauses.translateClauses modresult absyn in
    let _ = raise_on_error  "Translate clauses failed" in
    (* let () = Clauses.printTranslatedClauses clauses newclauses stdout in *)
    let absyn = Typereduction.reduceSkeletons absyn in
    let _ = raise_on_error  "Reduce skeleton failed" in
    let absyn = Processclauses.processClauses absyn clauses newclauses closeddefs in
    let _ = raise_on_error  "Process clauses failed" in
    let _ = Annvariables.processClauses absyn in
    let _ = raise_on_error  "Process clauses failed" in
    let cg = Codegen.generateModuleCode absyn in
    let _ = raise_on_error  "Generating code failed" in
    let module_name = Absyn.getModuleName absyn in
    let _  = prerr_endline ("module name " ^ module_name) in 
    let cwd = Unix.getcwd () in
    let _  = print_endline ("current working dir " ^ cwd) in 
    let _ = Module.setPath (cwd ^ "/") in 
    (* let _ = Hashtbl.replace ctx module_name temp_ in*)
    let bytecode_file = Bytecode.makeByteCodeFileName module_name in
    let _  = prerr_endline ("bytecode file " ^ bytecode_file) in 
    let _ = Bytecode.openOutChannel bytecode_file in
    let _ = raise_on_error  "Generating code failed" in
    let _ = Bytecode.setWordSize () in
    let _ = Spitcode.writeByteCode cg in
    let _ = Bytecode.closeOutChannel () in 
    let _ = raise_on_error  "Generating code failed" in
    let result = link module_name 2 in 
    let _  = prerr_endline ("link result " ^ (string_of_int result)) in 
    let ()  = flush_all () in
    (*
      let absyn_str = Absyn.Show_amodule.show absyn in
      let () = print_endline absyn_str in 
    *)
    (* let out_chan = open_out "absyn.txt" in
       let () = output_string out_chan absyn_str in 
       let () = close_out out_chan in *)
    Success("text/plain", code (*Absyn.Show_amodule.show absyn*) )
  with
    | CompileError descr -> 
        let () = flush_all () in 
        Error("CompileError", descr, [])
    | Parsing.Parse_error ->
        let () = flush_all () in
        Error("Parsing.Parse_error", "", [])
    | e ->
        let ()  = flush_all () in
        Error("Unkonwn error!", (Printexc.to_string e), [])
          

let input_more_results shell = 
  let answer = Kernel.shell_raw_input shell "More results?" in
  match answer with
      "y" -> true
    | _ -> false

        
let handle_query_program shell ctx code m =
   try
     let env_dir = Unix.getcwd () (*Hashtbl.find ctx m*) in
     (* Front.systemInit 0; *)
     Module.setPath (env_dir ^ "/");
     Module.moduleLoad m;
     Front.simulatorInit () ; 
     Module.moduleInstall m;
     Module.initModuleContext () ;

     (* TODO
         - interactive mode
         - simple callback is not available
         - multiple outs are ok
         - but input/output coupling is aslo interesting
         - state on server? / state on client?
     *)
     let max_solutions = 10 in
     let result = ref "" in
     let rec solve_query_batch_aux numResults =
       if Query.solveQuery () && numResults < max_solutions then
         let rec answers_to_string = function
           | (var,term) :: rest ->  "(" ^ var ^ "," ^ term ^ ") " ^ (answers_to_string rest)
           | [] -> ""
         in
         let () = Query.showAnswers () in
         (*result := !result ^ answers_to_string answers ^ "\n";*)
         solve_query_batch_aux numResults + 1           
       else
         numResults
     in
     let result = 
       if Query.buildQueryTerm code (Module.getCurrentModule ()) then
         begin
           solve_query_batch_aux 0 ;
           Success("text/plain", !result)
         end
       else
         Error("Error_buildQueryTerm", code, [])
     in
     Module.cleanModule ();
     Front.simulatorReInit false;
     Module.initModuleContext ();
     result
   with 
     | e ->
         let ()  = prerr_endline ("Error " ^ (Printexc.to_string e) ^ " module " ^ m) in
         let ()  = flush_all () in
         Error("Error", (Printexc.to_string e) ^ " module " ^ m , [])

              
module TeyjusHandler = 
struct
  type ctx_t = (string, string) Hashtbl.t
  let execute_request shell ctx request = 
    try 
      let code = get_code request.content in 
      let input = get_input request.content "program" in
      let m = get_module request.content "test" in
      match code with
          "1"  -> Success("text/plain", "1")
        | code ->
            match input with 
                "program" -> 
                  handle_compile_program shell ctx code m
              | "query" ->
                  handle_query_program shell ctx code m
    with
      | e ->
          let ()  = flush_all () in
          Error("Unkonwn error!", (Printexc.to_string e), [])
            
end
  
module TeyjusIPython = Kernel.IPython(TeyjusHandler) 
    



(* 
   main entry 
 *)
let () =
  Front.systemInit 0;
  (* Front.simulatorInit (); *)
  Kernel.env_init Sys.executable_name ;
  Printexc.record_backtrace true; 
  let kernel = (TeyjusIPython.init_kernel
                         (Array.to_list Sys.argv) 
                         (*ctx*) (Hashtbl.create 44) 
                         TeyjusHandler.execute_request) in
  while not (Kernel.has_shutdown kernel) do
    Unix.sleep 1
  done;
  Kernel.free kernel
    
