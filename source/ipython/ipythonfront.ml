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

module type TeyjusHandler = sig
  type ctx_t 
  val execute_request : ctx_t -> Kernel.execute_request_t -> Kernel.execute_response_t
end


let compile parse lexbuf filename =
  let () = Lplex.setFileName lexbuf filename in
  let result = parse Lplex.initial lexbuf in
  result

let suffix str len = (String.sub str len ((String.length str) - len))
    
  
module TeyjusHandler = 
struct
  type ctx_t = unit

  let execute_request ctx request =
    match request.code with
        "1"  -> Success("text/plain", "1")
      | code ->
          if (String.length code) > 5 && (String.sub code 0 6) = "parse " then
            try
              let (sigresult, modresult) = compile Lpyacc.parseSigMod (Lexing.from_string (suffix request.code 6)) "test.mod" in
              let (absyn, sigabsyn) = Translate.translate modresult sigresult in
              Success("text/plain", Absyn.Show_amodule.show absyn)
            with
              | Parsing.Parse_error ->
                  Error("Parsing.Parse_error", "", [])

          else if (String.length code) > 11 && (String.sub code 0 12) = "parsestring " then
            match Compile.compileString (suffix code 12) with
                None -> Error("compileStringError", "failed to parse " ^ (suffix code 12), [])
              | Some value -> Success("text/plain", "parsed " ^ (suffix code 12))
          else
            Error("Internal", "Failed to handle input", [])
end

module TeyjusIPython = Kernel.IPython(TeyjusHandler) 
    

(* 
   main entry 
 *)
let () =
  let test_shutdown = (TeyjusIPython.init_kernel
                         (Array.to_list Sys.argv) () TeyjusHandler.execute_request) in
  while not (test_shutdown()) do
    Unix.sleep 1
  done;
    


