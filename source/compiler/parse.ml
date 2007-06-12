type pos = Errormsg.pos
type symbol = Symbol.symbol

(*  Terms and Terms with Free Variables *)
type ptvarlist = Absyn.atypesymbol list
type ptterm = Term of (Absyn.aterm * Types.typemolecule)
type pttermandvariables = TermAndVariables of (ptterm * ptvarlist)

(*  Functions indicating what to do when a new constant or kind is encountered. *)
type ptnewconstant = symbol -> Absyn.aconstant Table.SymbolTable.t -> Absyn.aconstant Table.SymbolTable.t
type ptnewkind = symbol -> int -> pos -> Absyn.akind Table.SymbolTable.t -> Absyn.akind Table.SymbolTable.t

(*  Term Accessors  *)
let getTermTerm = function Term(t, _) -> t
let getTermMolecule = function Term(_, mol) -> mol
let getFixedTermTerm = function Term(t, _) -> t
let getFixedTermTypeMolecule = function Term(_, mol) -> mol
let getTermAndVariablesTerm = function TermAndVariables(t, _) -> t
let getTermAndVariablesVariables = function TermAndVariables(_, vars) -> vars

let errorTerm = Term(Absyn.errorTerm, Types.errorMolecule)
let errorTermAndVariables = TermAndVariables(errorTerm, [])

(**********************************************************************
*Term-parsing types.
**********************************************************************)
(*  Stack and stack items.  *)
type ptstackdata =
  StackTerm of (ptterm)
| StackOp of (Absyn.aconstant * Absyn.atype list * pos)
| StackError

(*  Parser State  *)
type ptparsestate =
  PrefixState
| PrefixrState
| PrefixWithArgState
| PrefixrWithArgState
| InfixState
| InfixrState
| InfixWithArgState
| InfixrWithArgState
| PostfixState
| TermState
| NoneState
| ErrorState

let string_of_parserstate = function
  PrefixState -> "PrefixState"
| PrefixrState -> "PrefixrState"
| PrefixWithArgState -> "PrefixWithArgState"
| PrefixrWithArgState -> "PrefixrWithArgState"
| InfixState -> "InfixState"
| InfixrState -> "InfixrState"
| InfixWithArgState -> "InfixWithArgState"
| InfixrWithArgState -> "InfixrWithArgState"
| PostfixState -> "PostfixState"
| TermState -> "TermState"
| NoneState -> "NoneState"
| ErrorState -> "ErrorState"

type ptstack = Stack of (ptstackdata list * ptparsestate * int * Absyn.afixity)

let getStackItemPos = function
    StackTerm(t) -> (Absyn.getTermPos (getTermTerm t))
  | StackOp(_,_,p) -> p
  | StackError -> (Errormsg.impossible Errormsg.none "Parse.getStackItemPos: stack error.")

let getStackTermTerm = function
    StackTerm(t) -> t
  | s -> Errormsg.impossible (getStackItemPos s) "getStackTermTerm: invalid stack term."

let getStackOpConstant = function
    StackOp(c,_,_) -> c
  | s -> Errormsg.impossible (getStackItemPos s) "getStackOpConstant: invalid stack op."

let getStackOpEnv = function
    StackOp(_,e,_) -> e
  | s -> Errormsg.impossible (getStackItemPos s) "getStackOpEnv: invalid stack op."

let getStackOpFixity = function
    StackOp(c,_,_) -> (Absyn.getConstantFixity c)
  | _ -> Errormsg.impossible Errormsg.none "getStackOpFixity: invalid stack op."

let getStackOpPrec = function
    StackOp(c,_,_) -> (Absyn.getConstantPrec c)
  | _ -> Errormsg.impossible Errormsg.none "getStackOpPrec: invalid stack op."

(*  Stack Accessors *)
let getStackStack = function Stack(data,_,_,_) -> data
let getStackState = function Stack(_,state,_,_) -> state
let getStackPrec = function Stack(_,_,prec,_) -> prec
let getStackFixity = function Stack(_,_,_,fix) -> fix

let printStack stack =
  let rec print' data =
    match data with
      (StackOp(c,_,_))::ds -> "{" ^ (Absyn.getConstantName c) ^ "}" ^ (print' ds)
    | (StackTerm(t))::ds -> "{" ^ (Absyn.string_of_term (getTermTerm t)) ^ "}" ^ (print' ds)
    | (StackError)::ds -> "{StackError}" ^ (print' ds)
    | [] -> ""
  in
  
  (*
  let data = getStackStack stack in
  let str = (print' data) in
  Errormsg.log Errormsg.none ("Stack: " ^ str)
  *)
  
  ()

let contains env v =
  let find tsym =
    (Absyn.getTypeSymbolSymbol tsym) = v
  in
  (List.exists find env)

let get = fun env v ->
  let find tsym =
    (Absyn.getTypeSymbolSymbol tsym) = v
  in
  
  try
    List.find find env
  with 
    Not_found ->
      Errormsg.impossible Errormsg.none "Parse.get: entry not found"

let add = fun env sym tsym ->
  if (contains env sym) then
    env
  else
    tsym::env


let newStack = Stack([], NoneState, 0, Absyn.NoFixity)
let errorStack = Stack([], ErrorState, 0, Absyn.NoFixity)

exception TermException

(**********************************************************************
* Beta-normalization types.
**********************************************************************)
type environmententry =
    TermEntry of (Absyn.aterm)
  | SuspensionEntry of (Absyn.aterm * environmentcell list)

and environmentcell =
    EnvironmentCell of (Absyn.atypesymbol * environmententry ref)

(**********************************************************************
*Error functions.
**********************************************************************)
let idTypeError = fun tmol result pos ->
  let info =
    Errormsg.info ("Type previously determined for variable: " ^ (Types.string_of_typemolecule tmol))
  in
  
  match result with
    Types.Success -> Errormsg.impossible Errormsg.none "Parse.idTypeError: unexpected unification result"
  | Types.OccursCheckFailure -> Errormsg.error pos ("circularity discovered during type matching" ^ info)
  | Types.ClashFailure -> Errormsg.error pos ("incompatibility discovered during type matching" ^ info)

let constantTypeError = fun tmol result pos ->
  let info =
    Errormsg.info ("Type previously defined or determined for constant: " ^ (Types.string_of_typemolecule tmol))
  in
  
  match result with
    Types.Success -> Errormsg.impossible Errormsg.none "Parse.constantTypeError: unexpected unification result"
  | Types.OccursCheckFailure -> Errormsg.error pos ("circularity discovered during type matching" ^ info)
  | Types.ClashFailure -> Errormsg.error pos ("incompatibility discovered during type matching" ^ info)

(**********************************************************************
*makeType:
* Build a term and type representation of a term with a given (named
* pervasive) type.
**********************************************************************)
let makeType = fun term s ktable pos ->
  match (Table.find (Symbol.symbol s) ktable) with
    Some(k) ->
      let ty = Types.makeKindMolecule k in
      Term(term, ty)
  | None -> (Errormsg.impossible (Absyn.getTermPos term) ("Parse.makeType: invalid kind " ^ s))

let makeConstantTerm c pos =
  let ty = Types.makeConstantMolecule c in
  let env = Types.getMoleculeEnvironment ty in
  Term(Absyn.ConstantTerm(c,env,false,pos), ty)

(**********************************************************************
*stackTop:
* Returns the top of the given stack.
**********************************************************************)
let stackTop = function
  Stack(s::_,_,_,_) -> s
| Stack([],_,_,_) -> (Errormsg.impossible Errormsg.none "Parse.stackTop: stack is empty.")

(**********************************************************************
*popStack:
* Removes the top of the given stack and returns the new stack.
**********************************************************************)
let popStack = function
  Stack(_::ss,a,b,c) -> Stack(ss, a, b, c)
| Stack([],_,_,_) -> (Errormsg.impossible Errormsg.none "Parse.popStack: stack is empty.")

(**********************************************************************
*translateClauses:
* Given an abstract syntax representation of a module and a list of
* preabsyn terms, generates a list of absyn clauses.
**********************************************************************)
let rec translateClause = fun term amodule newconstant newkind ->
  let (tv, _, _, _) = parseTerm term [] [] amodule newStack in
  let tv' = (getTermAndVariablesTerm tv) in
  let term' = getTermTerm tv' in
  let type' = getTermMolecule tv' in
  
  (*  Remove all overloaded operators.  *)
  let term'' = removeOverloads term' in

  (*  Ensure that the term is valid and is of the correct type. *)
  if term'' = Absyn.errorTerm then
    Absyn.errorTerm
  else
  
  (*  Make sure it is of type o.  *)
  let boolkind = Pervasive.kbool in
  let booltype = Types.makeKindMolecule boolkind in
  
  if (Types.unify type' booltype) <> (Types.Success) then
    (Errormsg.error (Absyn.getTermPos term') ("expecting term of type: o" ^ 
      (Errormsg.info "encountered term:") ^
      (Errormsg.info (Absyn.string_of_term term'')) ^
      (Errormsg.info "of type:") ^
      (Errormsg.info (Types.string_of_typemolecule type')));
    Absyn.errorTerm)
  else
  
  (*  Normalize the term. *)
  let term''' = (normalizeTerm term'') in
  
  let _ = Errormsg.log Errormsg.none ("Before removing nested: " ^ (Absyn.string_of_term_ast term''')) in
  (*  Remove nested abstractions  *)
  let term'''' = (removeNestedAbstractions term''') in
  let _ = Errormsg.log Errormsg.none ("After removing nested: " ^ (Absyn.string_of_term_ast term'''')) in
  term''''

(**********************************************************************
*translateTerm:
* Given an abstract syntax representation of a module and a preabsyn
* term, generates a normalized absyn term.
**********************************************************************)
and translateTerm = fun term amodule ->
  let _ = Errormsg.log (Preabsyn.getTermPos term) ("unparsed preabsyn: " ^ (Preabsyn.string_of_term term)) in
  let (tv,_,_,_) = parseTerm term [] [] amodule newStack in
  let term' = getTermTerm (getTermAndVariablesTerm tv) in
  let mol' = getTermMolecule (getTermAndVariablesTerm tv) in
  let _ = Errormsg.log (Absyn.getTermPos term') ("parsed term: " ^ (Absyn.string_of_term term') ^ " : " ^ (Types.string_of_typemolecule mol')) in
  let term'' = removeOverloads term' in
  let term''' = normalizeTerm term'' in
  let _ = Errormsg.log (Absyn.getTermPos term''') ("parsed, normalized term: " ^ (Absyn.string_of_term term''')) in
  let fixedterm = fixTerm term''' in
  (Errormsg.log (Absyn.getTermPos term'') ("parsed, normalized, fixed term: " ^ (Absyn.string_of_term fixedterm));
  fixedterm)

(**********************************************************************
*parseTerm:
* Translate a single term from preabsyn to absyn.  Requires a constant
* table containing, at least, the pervasives; a kind table also containing
* at least the prevasives; and an abbreviation table containing at least
* the pervasives.  These are given in an absyn representation of a
* module.
**********************************************************************)
and parseTerm = fun term fvs bvs amodule stack ->
  let _ = printStack stack in
  match term with
    Preabsyn.SeqTerm([term], pos) -> (parseTerm term fvs bvs amodule stack)
  | Preabsyn.SeqTerm(terms, pos) -> (parseTerms terms fvs bvs amodule (reduceToTerm false) newStack)
  | Preabsyn.ListTerm(terms, pos) ->
      let terms' = terms @ [Preabsyn.IdTerm(Symbol.symbol "::", None, Preabsyn.ConstID, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol "nil", None, Preabsyn.ConstID, Errormsg.none)] in
      (parseTerms terms' fvs bvs amodule (reduceToTerm true) newStack)
  
  (*  | Preabsyn.ListTerm(terms, pos) -> (parseTerms terms fvs bvs amodule (reduceToListTerm (Some(makeConstantTerm Pervasive.nilConstant pos))) newStack)  *)
  
  | Preabsyn.ConsTerm(headterms, tailterm, pos) ->
      (*  Translate the tail term first, then translate the head with respect to the new
          set of free variables.  Reduce using the tail term. *)
      (*
      let (tv, _, bvs', stack') = (parseTerm tailterm fvs bvs amodule stack) in
      let TermAndVariables(term', fvs') = tv in
      (parseTerms headterms fvs' bvs' amodule (reduceToListTerm (Some term')) newStack)
      *)
      let terms' = headterms @ [Preabsyn.IdTerm(Symbol.symbol "::", None, Preabsyn.ConstID, Errormsg.none); tailterm] in
      (parseTerms terms' fvs bvs amodule (reduceToTerm true) newStack)

  | Preabsyn.LambdaTerm(b, t, pos) ->
      let bbvs = parseTypeSymbols b amodule in
      let (tv', fvs', bvs', stack') = parseTerms t fvs (bbvs @ bvs) amodule (reduceToTerm false) newStack in
      let term' = getTermAndVariablesTerm tv' in
      (TermAndVariables(makeAbstraction term' bbvs pos, fvs'), fvs', bvs', stack)
  | Preabsyn.IntTerm(i, pos) -> (TermAndVariables((makeType (Absyn.IntTerm(i, false, pos)) "int" (Absyn.getModuleKindTable amodule) pos), fvs), fvs, bvs, stack)
  | Preabsyn.RealTerm(r, pos) -> (TermAndVariables((makeType (Absyn.RealTerm(r, false, pos)) "real" (Absyn.getModuleKindTable amodule) pos), fvs), fvs, bvs, stack)
  | Preabsyn.StringTerm(s, pos) -> (TermAndVariables((makeType (Absyn.StringTerm(Absyn.StringLiteral(s), false, pos)) "string" (Absyn.getModuleKindTable amodule) pos), fvs), fvs, bvs, stack)
  | Preabsyn.IdTerm(sym, ty, idkind, pos) ->
      let (op', fvs', bvs') = (translateId term fvs bvs amodule) in
      (match op' with
        StackOp(_) -> 
          (Errormsg.error pos ("operator used without necessary arguments");
          (makeError fvs, fvs', bvs', errorStack))
      | StackTerm(t) -> (TermAndVariables(t, fvs'), fvs', bvs', stack)
      | StackError -> (errorTermAndVariables, [], [], errorStack))
  | Preabsyn.ErrorTerm -> (Errormsg.impossible Errormsg.none "Parse.parseTerm: error term encountered.")

(**********************************************************************
*parseTerms:
* Translate a list of preabsyn terms into abstract syntax.  Executes
* the given operation upon successful translation of the list.
**********************************************************************)
and parseTerms terms fvs bvs amodule oper stack =
  (********************************************************************
  *translate':
  * Translate an individual term in the list.
  ********************************************************************)
  let translate' = fun t ->
    (******************************************************************
    *simple:
    * Translate the current term as usual and attempt to stack it.
    ******************************************************************)
    let simple () =
      let (term', fvs', bvs', stack') = (parseTerm t fvs bvs amodule stack) in

      try
        ((stackTerm (getTermAndVariablesTerm term') false amodule stack), fvs', bvs')
      with
        TermException -> (errorStack, bvs, fvs)
    in
    
    (match t with
      Preabsyn.SeqTerm(_) -> simple ()
    | Preabsyn.ListTerm(_) -> simple ()
    | Preabsyn.ConsTerm(_) -> simple ()
    | Preabsyn.IntTerm(_) -> simple ()
    | Preabsyn.RealTerm(_) -> simple ()
    | Preabsyn.StringTerm(_) -> simple ()
    | Preabsyn.IdTerm(_) ->
        let (ot, fvs', bvs') = (translateId t fvs bvs amodule) in
        (try
          (match ot with
            StackOp(_) -> ((stackOperation ot false amodule stack), fvs', bvs')
          | StackTerm(t) -> ((stackTerm t false amodule stack), fvs', bvs')
          | StackError -> (errorStack, fvs', bvs'))
        with
          TermException -> (errorStack, fvs', bvs'))
    | Preabsyn.LambdaTerm(_) -> simple ()
    | Preabsyn.ErrorTerm -> (errorStack, fvs, bvs))
  in

  (*  Translate each term in turn.  Once the end of the list has been
      reached, execute the given termination operation. *)
  let _ = printStack stack in
  match terms with
    (t::ts) ->
      let (stack', fvs', bvs') = translate' t in
      (parseTerms ts fvs' bvs' amodule oper stack')
  | [] -> (oper fvs bvs amodule stack)

(**********************************************************************
*parseTypeSymbols:
* Parses a list of type symbols.
**********************************************************************)
and parseTypeSymbols tsymlist amodule =
  let newBoundTypeSymbol sym ty pos =
    match ty with
      Some t ->
        let t' = Translate.translateType t amodule in
        Absyn.BoundVar(sym, ref None, ref true, ref(Some t'))
    | None ->
        Absyn.BoundVar(sym, ref None, ref true,ref(Some (Absyn.makeTypeVariable ())))
  in

  let rec parse' tsymlist =
    match tsymlist with
      (Preabsyn.TypeSymbol(sym, ty, k, pos))::tsymlist' ->
        let bv = newBoundTypeSymbol sym ty pos in
        bv :: (parse' tsymlist')
    | [] -> []
  in
  parse' tsymlist

(**********************************************************************
*translateId:
* Converts a preabsyn ID to an absyn representation.  How it does so
* depends on what sort of ID it is.  A better description is in order.
**********************************************************************)
and translateId term fvs bvs amodule =
  match term with
    Preabsyn.IdTerm(sym, ty, k, pos) ->
      (*  Anonymous Variable  *) 
      if k = Preabsyn.AVID then
        (makeVarToOpTerm term fvs bvs amodule makeAnonymousTypeSymbol)
      else

      (*  Variable  *)
      if k = Preabsyn.VarID then
        if (contains fvs sym) then
          (varToOpTerm term (get fvs sym) fvs bvs amodule (Absyn.makeFreeVarTerm))
        else
          (makeVarToOpTerm term fvs bvs amodule makeAnonymousTypeSymbol)
      else

      if (contains bvs sym) then
        (varToOpTerm term (get bvs sym) fvs bvs amodule (Absyn.makeBoundVarTerm))
      else

      let o = (Table.find sym (Absyn.getModuleConstantTable amodule)) in
      if Option.isSome o then
        let c = Option.get o in
        (constantToOpTerm term c fvs bvs amodule)
      else
        if k = Preabsyn.CVID then
          if (contains fvs sym) then
            (varToOpTerm term (get fvs sym) fvs bvs amodule Absyn.makeFreeVarTerm)
          else
            (makeVarToOpTerm term fvs bvs amodule makeImplicitTypeSymbol)
        else
          (*  At this point the constant is assumed to be unknown.
              Simply raise an error.  *)
          (Errormsg.error pos ("undeclared constant " ^ (Symbol.name sym));
          (StackError, fvs, bvs))
  | _ -> (Errormsg.impossible (Preabsyn.getTermPos term) "Parse.translateId: invalid term")

and constantToOpTerm = fun term constant fvs bvs amodule ->
  let make' = fun tmol pos ->
    let fixity = Absyn.getConstantFixity constant in
    if fixity = Absyn.NoFixity then
      (StackTerm(Term(Absyn.ConstantTerm(constant, (Types.getMoleculeEnvironment tmol), false, pos), tmol)), fvs, bvs)
    else
      (StackOp(constant, (Types.getMoleculeEnvironment tmol), pos), fvs, bvs)
  in
  match term with
    Preabsyn.IdTerm(sym, Some(ty), k, pos) ->
      let tm1 = Types.makeConstantMolecule constant in
      let tm2 = Types.Molecule((Translate.translateType ty amodule), []) in
      
      let result = (Types.unify tm1 tm2) in
      if result = Types.Success then
        (make' tm1 pos)
      else
        (constantTypeError tm1 result pos;
        (StackError, fvs, bvs))
  | Preabsyn.IdTerm(sym, ty, k, pos) ->
      let tm1 = Types.makeConstantMolecule constant in
      (make' tm1 pos)
  | _ -> Errormsg.impossible (Preabsyn.getTermPos term) "Parse.constantToOpTerm: invalid term."

and makeImplicitTypeSymbol = fun c sym ty ->
  Absyn.ImplicitVar(sym, ref c, ref false, ref (Some ty))
and makeBoundTypeSymbol = fun c sym ty ->
  Absyn.BoundVar(sym, ref c, ref false, ref (Some ty))
and makeAnonymousTypeSymbol = fun c sym ty ->
  Absyn.AnonymousImplicitVar(sym, ref c, ref false, ref (Some ty))

and makeVarToOpTerm = fun term fvs bvs amodule makeSymFunc ->
  match term with
    Preabsyn.IdTerm(sym, Some(ty), k, pos) ->
      let skel = Translate.translateType ty amodule in
      let tmol = Types.Molecule(skel, []) in
      let typesym = makeSymFunc None sym skel in
      let fvs' = (add fvs sym typesym) in
      (StackTerm(Term(Absyn.makeFreeVarTerm typesym pos, tmol)), fvs', bvs)
  | Preabsyn.IdTerm(sym, None, k, pos) ->
      let skel = Absyn.makeTypeVariable () in
      let tmol = Types.Molecule(skel, []) in
      let typesym = makeSymFunc None sym skel in
      let fvs' = (add fvs sym typesym) in
      (StackTerm(Term(Absyn.makeFreeVarTerm typesym pos, tmol)), fvs', bvs)
  | _ -> Errormsg.impossible Errormsg.none "Parse.makeVarToOpTerm: invalid id term"

and varToOpTerm = fun term typesym fvs bvs amodule makeVarFunc ->
  match term with
    Preabsyn.IdTerm(sym, Some(ty), k, pos) ->
      (*  If the term has a given type, attempt to unify it with the
          type associated with the given symbol.  *)  
      let tm1 = Types.Molecule(Absyn.getTypeSymbolRawType typesym, []) in
      let tm2 = Types.Molecule((Translate.translateType ty amodule), []) in
      let result = (Types.unify tm1 tm2) in
      if result = Types.Success then
        (StackTerm(Term(makeVarFunc typesym pos, tm2)), fvs, bvs)
      else
        (idTypeError tm1 result pos;
        (StackError, fvs, bvs))

  | Preabsyn.IdTerm(sym, None, k, pos) ->
     (*  If the term has no given type, simply create a new type variable and
         bind it.  *)
     let tm1 = Types.Molecule(Absyn.getTypeSymbolRawType typesym, []) in
     (StackTerm(Term(makeVarFunc typesym pos, tm1)), fvs, bvs)
  | _ -> Errormsg.impossible (Preabsyn.getTermPos term) "Parse.varToOpTerm: invalid term"

(**********************************************************************
*reduceToTerm:
* Reduces the given stack to a term.  Commas encountered in this mode
* are interpereted as conjunctions.
**********************************************************************)
and reduceToTerm = fun list fvs bvs amodule stack ->
  (********************************************************************
  *reduce':
  * Reduces the term.
  ********************************************************************)
  let rec reduce' = fun stack ->
      match (getStackState stack) with
        TermState ->
          let term = (getStackTermTerm (stackTop stack)) in
          (TermAndVariables(term, fvs), fvs, bvs, newStack)
      | ErrorState ->
          ((makeError fvs), fvs, bvs, errorStack)
      | _ ->
          try
            let stack' = reduceOperation list amodule stack in
            (reduce' stack')
          with 
            TermException -> ((makeError fvs), fvs, bvs, errorStack)
  in
  
  (*  Called when the top of the stack indicates an error.  *)
  let err = fun () ->
    let term = (getTermTerm (getStackTermTerm (stackTop stack))) in
    let pos = Absyn.getTermPos term in
    let fixity = (Absyn.string_of_fixity (Absyn.getConstantFixity (getStackOpConstant (stackTop stack)))) in
    
    (Errormsg.error pos ("missing right argument for " ^ fixity ^ " operator");
    (makeError fvs, fvs, bvs, errorStack))
  in

  match (getStackState stack) with
    PrefixState -> (err ())
  | PrefixrState -> (err ())
  | InfixState -> (err ())
  | InfixrState -> (err ())
  | _ -> (reduce' stack)

(**********************************************************************
*reduceToListTerm:
* Reduces the stack to a list term.  Commas encountered in this mode
* are interpereted as list separators.  Appends the given term to the
* parsed list.
**********************************************************************)
(*
and reduceToListTerm = fun list fvs bvs amodule stack ->
  let rec reduce' = fun stack ->
    let default = fun () ->
      try
        let stack' = (reduceOperation list amodule stack) in
        (reduce' stack')
      with
        TermException -> (makeError fvs, fvs, bvs, errorStack)
    in
    match (getStackState stack) with
      TermState ->
        let term' = (getStackTermTerm (stackTop stack)) in
        (TermAndVariables(term', fvs), fvs, bvs, newStack)
    | InfixrWithArgState ->
        let top = List.hd (getStackStack stack) in
        let ot = List.hd (List.tl (getStackStack stack)) in
        let rest = List.tl (List.tl (getStackStack stack)) in
        
        let item = Option.get list in
        if (getStackOpConstant ot) = (Pervasive.consConstant) then
          let pos = getStackItemPos top in
          let cons = (makeConstantTerm Pervasive.consConstant pos) in
          let term' = makeBinaryApply cons (getStackTermTerm top) item in
          let stack' = Stack((StackTerm(term'))::ot::rest, getStackState stack, getStackPrec stack, getStackFixity stack) in
          (reduceToTerm fvs bvs amodule stack')
        else
          default ()
    | _ -> default ()
  in
  
  (*  Called when the top of the stack indicates an error.  *)
  let err = fun () ->
    let top = (stackTop stack) in
    let pos = getStackItemPos top in
    let c = getStackOpConstant top in
    let fix = (Absyn.string_of_fixity (Absyn.getConstantFixity c)) in
    (Errormsg.error pos ("missing right argument for " ^ fix ^ " operator");
    (makeError fvs, fvs, bvs, errorStack))
  in
  
  match (getStackState stack) with
    PrefixState -> (err ())
  | PrefixrState -> (err ())
  | InfixState -> (err ())
  | InfixrState -> (err ())
  | _ ->
    let item = Option.get list in
    let (term, fvs', bvs', stack') = (reduce' stack) in
    let pos = (Absyn.getTermPos (getTermTerm (getTermAndVariablesTerm term))) in
    let cons = (makeConstantTerm Pervasive.consConstant pos) in
    let term' = makeBinaryApply cons (getTermAndVariablesTerm term) item in
    (TermAndVariables(term', fvs'), fvs', bvs', stack')
*)

(**********************************************************************
*reduceOperation:
**********************************************************************)
and reduceOperation = fun list amodule stack ->
  (********************************************************************
  *reduce':
  ********************************************************************)
  let reduce' = fun () ->
    let state = getStackState stack in
    let prec = getStackPrec stack in
    let fix = getStackFixity stack in
    
    let top = List.hd (getStackStack stack) in
    let prev = List.hd (List.tl (getStackStack stack)) in
    let rest = List.tl (List.tl (getStackStack stack)) in

    let reducePrefix = fun () ->
      let c = (makeConstantTerm (getStackOpConstant prev) (getStackItemPos prev)) in
      let newTop = StackTerm(makeApply c (getStackTermTerm top)) in
      let stack' = Stack(newTop::rest, state, prec, fix) in
      newParseState stack'
    in
    
    let reduceInfix = fun () ->
      let r = List.hd rest in
      let rest' = List.tl rest in

      let c = (makeConstantTerm (getStackOpConstant prev) (getStackItemPos prev)) in
      let newTop = StackTerm(makeBinaryApply c (getStackTermTerm r) (getStackTermTerm top)) in
      let stack' = Stack(newTop::rest', state, prec, fix) in
      newParseState stack'
    in
    
    match state with
      PostfixState ->
        let c = (makeConstantTerm (getStackOpConstant top) (getStackItemPos top)) in
        let newTop = StackTerm(makeApply c (getStackTermTerm prev)) in
        let stack' = Stack(newTop::rest, state, prec, fix) in
        newParseState stack'
    | PrefixWithArgState -> reducePrefix ()
    | PrefixrWithArgState -> reducePrefix ()
    | InfixWithArgState -> reduceInfix ()
    | InfixrWithArgState -> reduceInfix ()
    | _ -> (Errormsg.impossible Errormsg.none "reduceOperation: invalid stack state")
  in
  
  (********************************************************************
  *reduceApply:
  * Reduces an "apply operator" on the stack.  An "apply operator" is
  * the dummy inserted between adjacent terms.  The apply operator is
  * removed from the stack, and the top two terms (not includeing the
  * dummy) are put into an applicaiton.
  ********************************************************************)
  let reduceApply = fun stack ->
    let top = List.hd (getStackStack stack) in
    let pp = List.hd (List.tl (List.tl (getStackStack stack))) in
    let rest = List.tl (List.tl (List.tl (getStackStack stack))) in

    let apply = makeApply (getStackTermTerm pp) (getStackTermTerm top) in
    let stack' = Stack((StackTerm(apply))::rest, (getStackState stack), (getStackPrec stack), (getStackFixity stack)) in
    newParseState stack'
  in
  
  (********************************************************************
  *reduceComma:
  * Reduces a comma operator, where the operator could be either a list
  * separator or conjunction.  If the list argument contains a term,
  * then the operator should be parsed as a list separator; otherwise,
  * it is a conjunction.
  ********************************************************************)
  let reduceComma = fun stack ->
    let top = List.hd (getStackStack stack) in
    let comma = List.hd (List.tl (getStackStack stack)) in
    let pp = List.hd (List.tl (List.tl (getStackStack stack))) in
    let rest = List.tl (List.tl (List.tl (getStackStack stack))) in

    match list with
      true ->
        let pos = (getStackItemPos comma) in
        let cons = makeConstantTerm Pervasive.consConstant pos in
        let newTop = makeBinaryApply cons (getStackTermTerm pp) (getStackTermTerm top) in
        let stack' = Stack((StackTerm(newTop))::rest, getStackState stack, getStackPrec stack, getStackFixity stack) in
        newParseState stack'
    | false ->
        let pos = getStackItemPos comma in
        let conj = makeConstantTerm Pervasive.andConstant pos in
        let newTop = makeBinaryApply conj (getStackTermTerm pp) (getStackTermTerm top) in
        let stack' = Stack((StackTerm(newTop))::rest, getStackState stack, getStackPrec stack, getStackFixity stack) in
        newParseState stack'
  in
  
  (*  First, check for the special case of a "pseudo-application".  In this case
      the reduction is slightly different than regular: the two arguments of the
      application are applied to eachother.
      
      Next, check for a comma.  If the list argument indicates that the parser
      is parsing a list (in bracket notation) the comma will be interpereted as
      a cons (::), otherwise it will be interpreted as a conjunction (,).      
  *)
  
  let ot = List.hd (List.tl (getStackStack stack)) in
  match ot with
    StackOp(c,_,_) -> 
                      let csym = (Absyn.getConstantSymbol c) in
                      if csym = (Absyn.getConstantSymbol Pervasive.genericApplyConstant) then
                        (reduceApply stack)
                      else if csym = (Absyn.getConstantSymbol Pervasive.andConstant) then
                        (reduceComma stack)
                      else
                        reduce' ()
  | _ -> (reduce' ())


(**********************************************************************
*stackTerm:
**********************************************************************)
and stackTerm = fun term list amodule stack ->
  (********************************************************************
  *stackApply:
  * Stacks a 'pseudo' apply operator.
  ********************************************************************)
  let stackApply = fun () ->
    let apply = (makeApplyOp (Absyn.getTermPos (getTermTerm term))) in
    let stack' = stackOperation apply list amodule stack in
    (pushTerm term stack')
  in

  (*  Check whether to add a pseudo application operator. *)
  let state = getStackState stack in
  match state with
    PrefixState -> (pushTerm term stack)
  | PrefixrState -> (pushTerm term stack)
  | InfixState -> (pushTerm term stack)
  | InfixrState -> (pushTerm term stack)
  | NoneState -> (pushTerm term stack)
  
  | PrefixWithArgState -> stackApply ()
  | PrefixrWithArgState -> stackApply ()
  | InfixWithArgState -> stackApply ()
  | InfixrWithArgState -> stackApply ()
  | PostfixState -> stackApply ()
  | TermState -> stackApply ()
  | _ -> Errormsg.impossible (Absyn.getTermPos (getTermTerm term)) ("Parse.stackTerm: invalid stack state " ^ (string_of_parserstate state))

(**********************************************************************
*stackOperation:
* Handles stacking an operator on the parse stack; this is where the
* main decisions of shifting/reducing get made.  Note that an apply
* "pseudo" operator may have to be inserted between two adjacent terms
* in the input to ensure the appropriate reduction at a later stage.
**********************************************************************)
and stackOperation = fun o list amodule stack ->
  let rec stackOperation' = fun stack ->
    let state = (getStackState stack) in
    let fixity = (getStackOpFixity o) in
    let prec = (getStackOpPrec o) in
    let pos = (getStackItemPos o) in
    
    (******************************************************************
    *preOrInf:
    * Handles the case where stack state is Prefix or Infix.
    ******************************************************************)
    let preOrInf = fun stack ->
      if (Absyn.isFixityPrefix fixity) && (prec > (getStackPrec stack)) then
        (pushOperation o stack)
      else if (Absyn.isFixityPrefix fixity) then
        (Errormsg.error pos "conflict in operator precedences.";
        raise TermException)
      else
        (Errormsg.error pos 
          ("missing left argument for " ^
          (Absyn.string_of_fixity fixity) ^
          " operator");
        raise TermException)
    in

    (******************************************************************
    *preOrInfr:
    * Handles the case where state is PrefixrState or InfixrState.
    ******************************************************************)
    let preOrInfr = fun stack ->
      if (Absyn.isFixityPrefix fixity) && (prec >= (getStackPrec stack)) then
        pushOperation o stack
      else if (Absyn.isFixityPrefix fixity) then
        (Errormsg.error pos "conflict in operator precedences.";
        raise TermException)
      else
        (Errormsg.error pos 
          ("missing left argument for " ^
          (Absyn.string_of_fixity fixity) ^
          " operator");
        raise TermException)
    in
    
    (******************************************************************
    *preOrInfWithArg:
    * Handles case where stack state is PrefixWithArgState or 
    * InfixWithArgState.
    ******************************************************************)
    let preOrInfWithArg = fun stack ->
      let pre = fun () ->
        let stack' = stackOperation (makeApplyOp pos) list amodule stack in
        (pushOperation o stack')
      in
      
      let infOrPost = fun () ->
        if prec > (getStackPrec stack) then
          pushOperation o stack
        else if prec < (getStackPrec stack) then
          let stack' = (reduceOperation list amodule stack) in
          stackOperation' stack'
       else
          (Errormsg.error pos "conflict in operator precedences(3)";
          raise TermException)
      in
      
      let inflOrPostl = fun () ->
        if prec > (getStackPrec stack) then
          pushOperation o stack
        else
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
      in
      
      match fixity with
        Absyn.Prefix
      | Absyn.Prefixr -> pre ()
      
      | Absyn.Infix
      | Absyn.Infixr
      | Absyn.Postfix -> infOrPost ()
      
      | Absyn.Infixl
      | Absyn.Postfixl -> inflOrPostl ()
      
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *preOrInfrWithArg:
    * Handles case where state is PrefixrWithArgState or
    * InfixrWithArgState.
    ******************************************************************)
    let preOrInfrWithArg = fun stack ->
      let pre = fun () ->
        let stack' = stackOperation (makeApplyOp pos) list amodule stack in
        (pushOperation o stack')
      in
      
      let infOrPost = fun () ->
        if prec >= (getStackPrec stack) then
          (pushOperation o stack)
        else
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
      in
      
      let inflOrPostl = fun () ->
        if prec > (getStackPrec stack) then
          pushOperation o stack
        else if prec < (getStackPrec stack) then
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
        else
          let _ = printStack stack in
          (Errormsg.error pos "conflict in operator precedences(4)";
          raise TermException)
      in
      
      match fixity with
        Absyn.Prefix
      | Absyn.Prefixr -> pre ()
      
      | Absyn.Infix
      | Absyn.Infixr
      | Absyn.Postfix -> infOrPost ()
      
      | Absyn.Infixl
      | Absyn.Postfixl -> inflOrPostl ()
      
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *post:
    * Handles the case where state is PostfixState.
    ******************************************************************)
    let post = fun stack ->
      let pre = fun () ->
        let stack' = stackOperation (makeApplyOp pos) list amodule stack in
        pushOperation o stack'
      in
      
      let infOrPost = fun () ->
        if prec < (getStackPrec stack) then
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
        else
          (Errormsg.error Errormsg.none "conflict in operator precedences";
          raise TermException)
      in
      
      let inflOrPostl = fun () ->
        if prec <= (getStackPrec stack) then
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
        else
          (Errormsg.error Errormsg.none "conflict in operator precedences";
          raise TermException)
      in

      match fixity with
        Absyn.Prefix
      | Absyn.Prefixr -> pre ()
      
      | Absyn.Infix
      | Absyn.Infixr
      | Absyn.Postfix -> infOrPost ()
      
      | Absyn.Infixl
      | Absyn.Postfixl -> inflOrPostl ()
      
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *termState:
    * Handles the case where state is TermState.
    ******************************************************************)
    let termState = fun stack ->
      let pre = fun () ->
        let stack' = stackOperation (makeApplyOp pos) list amodule stack in
        pushOperation o stack'
      in
      
      match fixity with
        Absyn.Prefix
      | Absyn.Prefixr -> pre ()
      | _ -> pushOperation o stack
    in
    
    match state with
      NoneState ->
        if (Absyn.isFixityPrefix fixity) then
          (Errormsg.error pos
            ("missing left argument for " ^
            (Absyn.string_of_fixity fixity) ^
            " operator");
          raise TermException)
        else
          (pushOperation o stack)
    | PrefixState
    | InfixState -> preOrInf stack
    
    | PrefixrState
    | InfixrState -> preOrInfr stack
    
    | PrefixWithArgState
    | InfixWithArgState -> preOrInfWithArg stack
    
    | PrefixrWithArgState
    | InfixrWithArgState -> preOrInfrWithArg stack
    
    | PostfixState -> post stack
    
    | TermState -> termState stack
    
    | _ -> Errormsg.impossible Errormsg.none ("Parse.stackOperation: invalid stack state " ^ (string_of_parserstate state))
  in
  (stackOperation' stack)
(**********************************************************************
*pushTerm:
* Pushes a term onto the stack.  Returns the new stack.
**********************************************************************)
and pushTerm = fun term stack ->
  let Stack(ss,state,f,p) = stack in
  let stackdata = (StackTerm(term))::ss in
  match (getStackState stack) with
    PrefixState -> Stack(stackdata,PrefixrWithArgState,f,p)
  | PrefixrState -> Stack(stackdata, PrefixrWithArgState,f,p)
  | InfixState -> Stack(stackdata,InfixWithArgState,f,p)
  | InfixrState -> Stack(stackdata,InfixrWithArgState,f,p)
  | NoneState -> Stack(stackdata,TermState,f,p)
  | _ -> Errormsg.impossible Errormsg.none ("Parse.pushTerm: invalid stack state " ^ (string_of_parserstate state))

and pushOperation = fun o stack ->
  let stackdata = o :: (getStackStack stack) in
  let prec = getStackOpPrec o in
  let fix = (getStackOpFixity o) in
  let pos = getStackItemPos o in
  match fix with
    Absyn.Prefix -> Stack(stackdata, PrefixState, prec, fix)
  
  | Absyn.Prefixr -> Stack(stackdata, PrefixrState, prec, fix)
  
  | Absyn.Infix
  | Absyn.Infixl -> Stack(stackdata, InfixState, prec, fix)
  
  | Absyn.Infixr -> Stack(stackdata, InfixrState, prec, fix)
  
  | Absyn.Postfix
  | Absyn.Postfixl -> Stack(stackdata, PostfixState, prec, fix)
  
  | Absyn.NoFixity -> (Errormsg.impossible pos "Parse.pushOperation: invalid fixity.")

(**********************************************************************
*newParseState:
*Producing a new state after a reduction on the parse stack. Note that
*in such a case the top of stack item must be a term and, if the stack has
*at least two items, the one below this must be a prefix or infix operator.
**********************************************************************)
and newParseState = fun stack ->
  let Stack(list,state, prec, fix) = stack in
  match list with
    (_::ot::_) ->
      let prec' = getStackOpPrec ot in
      let fix' = getStackOpFixity ot in
      (match fix' with
        Absyn.Prefix
      | Absyn.Prefixr -> Stack(list, PrefixrWithArgState, prec', fix')
      
      | Absyn.Infix
      | Absyn.Infixl -> Stack(list, InfixWithArgState, prec', fix')
      
      | Absyn.Infixr -> Stack(list, InfixrWithArgState, prec', fix')
      
      | _ -> Errormsg.impossible Errormsg.none "Parse.newParseState: invalid fixity")
  | _ ->
    Stack(list, TermState, prec, fix)

(**********************************************************************
*makeAbstraction:
* Makes an abstraction term by abstracting over the given term for all
* variables in bvs.
**********************************************************************)
and makeAbstraction termmol bvs pos =
  (********************************************************************
  *makeTerm:
  ********************************************************************)
  let rec makeTerm currentterm bvs =
    match bvs with
      [] -> currentterm
    | bv::bvs' ->
        let term' = Absyn.AbstractionTerm(
          Absyn.NestedAbstraction(bv, currentterm),
          false,
          pos) in
        (makeTerm term' bvs')
  in

  if (List.length bvs) == 0 then
    (Errormsg.impossible Errormsg.none "Parse.makeType: invalid number of bvs.")
  else
    let term = getTermTerm termmol in
    let term' = makeTerm term bvs in
    
    let mol = getTermMolecule termmol in
    let bvtypes = List.rev_map (Absyn.getTypeSymbolType) bvs in
    let ty' = Absyn.makeArrowType (Types.getMoleculeType mol) bvtypes in
    let env' = Types.getMoleculeEnvironment mol in
    Term(term', Types.Molecule(ty', env'))

(**********************************************************************
*makeError:
* Builds a term and type representation of an error.
**********************************************************************)
and makeError = fun fvs ->
  (TermAndVariables(errorTerm, fvs))

(**********************************************************************
*makeApplyOp:
* Create a generic application operator that should go between terms
* when two terms are encountered directly next to each other.
**********************************************************************)
and makeApplyOp = fun pos ->
  StackOp(Pervasive.genericApplyConstant, [], pos)
  
(**********************************************************************
*makeApply:
* Makes an application term, performing type checking.
**********************************************************************)
and makeApply = fun f arg ->
  let term = Absyn.ApplicationTerm(
    Absyn.CurriedApplication(getTermTerm f, getTermTerm arg),
    false,
    Absyn.getTermPos (getTermTerm f)) in
  let ty = Types.checkApply (getTermMolecule f) (getTermMolecule arg) term in
  
  if ty = Types.errorMolecule then
    errorTerm
  else
    Term(term, ty)

(**********************************************************************
*makeBinaryApply:
* Make an application term, performing type checking.  Special case
* of makeApply, used to display better error information.
**********************************************************************)
and makeBinaryApply = fun f (arg1 : ptterm) (arg2 : ptterm) ->
  let term = Absyn.ApplicationTerm(
    Absyn.CurriedApplication(
      Absyn.ApplicationTerm(
        Absyn.CurriedApplication(getTermTerm f, getTermTerm arg1),
        false,
        Absyn.getTermPos (getTermTerm f)),
      getTermTerm arg2),
    false,
    Absyn.getTermPos (getTermTerm f)) in
  
  let ty = Types.checkApply (getTermMolecule f) (getTermMolecule arg1) term in
  let ty' = Types.checkApply ty (getTermMolecule arg2) term in
  
  if ty' = Types.errorMolecule then
    errorTerm
  else
    Term(term, ty')

(**********************************************************************
*removeOverloads:
* Change the polymorphic overloaded operator constants to the correct
* monomorphic version.
**********************************************************************)
and removeOverloads term =
  match term with
    Absyn.IntTerm(_)
  | Absyn.RealTerm(_)
  | Absyn.StringTerm(_)
  | Absyn.FreeVarTerm(_)
  | Absyn.BoundVarTerm(_) -> term
  
  | Absyn.AbstractionTerm(Absyn.NestedAbstraction(t, body), b, p) ->
      let body' = removeOverloads body in
      Absyn.AbstractionTerm(Absyn.NestedAbstraction(t, body'), b, p)
  | Absyn.AbstractionTerm(Absyn.UNestedAbstraction(ts, c, body), b, p) ->
      let body' = removeOverloads body in
      Absyn.AbstractionTerm(Absyn.UNestedAbstraction(ts, c, body'), b, p)
  
  | Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, args, i), b, p) ->
      let f' = removeOverloads f in
      let args' = List.map removeOverloads args in
      Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f', args', i), b, p)
  | Absyn.ApplicationTerm(Absyn.CurriedApplication(f, a), b, p) ->
      let f' = removeOverloads f in
      let a' = removeOverloads a in
      Absyn.ApplicationTerm(Absyn.CurriedApplication(f', a'), b, p)

  | Absyn.ConstantTerm(c, tl, b, p) ->
      if Pervasiveutils.isOverloaded c then
        let ty = List.hd tl in
        let ty' = Absyn.dereferenceType ty in
        (match ty' with
          Absyn.TypeSetType(default, l, _) ->
            let k =
              (if (List.length (!l)) = 0 then
                Errormsg.impossible p "Parse.removeOverloads: invalid type set"
              else if (List.length (!l)) = 1 then
                Absyn.getTypeKind (List.hd !l)
              else
                Absyn.getTypeKind default) in
            Absyn.ConstantTerm(Pervasiveutils.getOverload k c, [], b, p)
        | _ -> Absyn.ErrorTerm)
      else
        term
  | Absyn.ErrorTerm -> Absyn.ErrorTerm

(**********************************************************************
*removeNestedAbstractions:
* Removes all nested abstractions in a term, converting them to an
* unnested abstraction.
**********************************************************************)
and removeNestedAbstractions term =
  (********************************************************************
  *rename:
  * Renames all occurences of a tsym to another tsym.
  ********************************************************************)
  let rec rename oldtsym newtsym term =
    match term with
      | Absyn.IntTerm(_)
      | Absyn.RealTerm(_)
      | Absyn.StringTerm(_)
      | Absyn.ConstantTerm(_)
      | Absyn.ErrorTerm -> term
      
      | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym),p,b) ->
          if tsym = oldtsym then
            Absyn.FreeVarTerm(Absyn.NamedFreeVar(newtsym),p,b)
          else
            term
      | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), p, b) ->
          if tsym = oldtsym then
            Absyn.BoundVarTerm(Absyn.NamedBoundVar(newtsym),p,b)
          else
            term
      
      | Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym,body),p,b) ->
          if (tsym = oldtsym) then
            Absyn.AbstractionTerm(Absyn.NestedAbstraction(newtsym,body),p,b)
          else
            term
      | Absyn.ApplicationTerm(Absyn.CurriedApplication(l,r),p,b) ->
          Absyn.ApplicationTerm(
            Absyn.CurriedApplication(
              rename oldtsym newtsym l,
              rename oldtsym newtsym r),
            p,b)
      | Absyn.ApplicationTerm(Absyn.FirstOrderApplication(head,args,i),p,b) ->
          Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(
              rename oldtsym newtsym head,
              List.map (rename oldtsym newtsym) args,
              i),
            p,b)
      | _ -> Errormsg.impossible Errormsg.none "Parse.rename: invalid term"
  in
  
  (********************************************************************
  *remove:
  * Remove the abstractions in a term.  The argument "abstractions"
  * contains the an option of the list of symbols to be abstracted over.
  * It starts as None, and remove serves to locate the first nested
  * abstraction.  From then on it grows with each passed nested abstraction.
  ********************************************************************)
  let rec remove term abstractions =
    match term with
        Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, body),b,p) ->
          if Option.isSome abstractions then
            let sym = Absyn.getTypeSymbolSymbol tsym in
            if (List.exists (fun tsym' -> ((Absyn.getTypeSymbolSymbol tsym') = sym)) (Option.get abstractions)) then
              let sym' = (Symbol.generateName (Symbol.name sym)) in
              let tsym' = Absyn.copyTypeSymbol sym' tsym in
              let body' = rename tsym tsym' body in
              let abs' = Some (tsym' :: (Option.get abstractions)) in
              remove body' abs'
            else
              let abs' = Some (tsym :: (Option.get abstractions)) in
              remove body abs'
          else
            let abs = Some([tsym]) in
            let (body', abs') = remove body abs in
            let tsyms = Option.get abs' in
            let term' = Absyn.AbstractionTerm(
              Absyn.UNestedAbstraction(tsyms , List.length tsyms, body'), b, p) in
            (term', None)
      | Absyn.AbstractionTerm(Absyn.UNestedAbstraction(_),_,_) ->
          (Errormsg.impossible Errormsg.none
            "Parse.removeNestedAbstractions: unexpected unnested abstraction")

      | Absyn.IntTerm(_)
      | Absyn.RealTerm(_)
      | Absyn.StringTerm(_)
      | Absyn.ConstantTerm(_)
      | Absyn.FreeVarTerm(_)
      | Absyn.BoundVarTerm(_)
      | Absyn.ErrorTerm -> (term, abstractions)
      
      | Absyn.ApplicationTerm(Absyn.CurriedApplication(t1,t2),b,p) ->
          let (t1', abs') = (remove t1 abstractions) in
          let (t2', abs'') = (remove t2 abs') in
          (Absyn.ApplicationTerm(Absyn.CurriedApplication(t1',t2'),b,p), abs'')
      | Absyn.ApplicationTerm(Absyn.FirstOrderApplication(head, args, i), b, p) ->
          let rec removeList args abstractions =
            (match args with
              [] -> ([], abstractions)
            | a::aa ->
                let (a', abs') = remove a abstractions in
                let (aa', abs'') = removeList aa abs' in
                (a'::aa', abs''))
          in
          let (head', abs') = (remove head abstractions) in
          let (args', abs'') = (removeList args abstractions) in
          (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(head', args', i), b, p), abs'')
  in
  let (term', _) = (remove term None) in
  term'

(**********************************************************************
*Beta normalization:
**********************************************************************)
and getEntryTerm = function
    TermEntry(t) -> t
  | _ -> Errormsg.impossible Errormsg.none "Parse.getEntryTerm: invalid entry"

and getEntrySuspensionTerm = function
    SuspensionEntry(t,_) -> t
  | _ -> Errormsg.impossible Errormsg.none "Parse.getEntrySuspensionTerm: invalid entry"
  
and getEntrySuspensionEnv = function
    SuspensionEntry(_,env) -> env
  | _ -> Errormsg.impossible Errormsg.none "Parse.getEntrySuspensionEnv: invalid entry"

and isEntryTerm = function
    TermEntry(_) -> true
  | _ -> false

and isEntrySuspension = function
    SuspensionEntry(_) -> true
  | _ -> false

and getCellEntry = function
  EnvironmentCell(_, entry) -> entry

(**********************************************************************
*normalizeTerm:
* This is the main routine for beta-normalizing an abstract syntax
* term.
**********************************************************************)
and normalizeTerm = fun term ->
  let makeSuspension = fun tsym term env tail ->
    EnvironmentCell(tsym, ref (SuspensionEntry(term, env))) :: tail
  in
  
  let makeTerm = fun tsym term tail ->
    EnvironmentCell(tsym, ref (TermEntry(term))) :: tail
  in
  
  let getEnvironmentSize = List.length in
  let emptyEnvironment = [] in
  
  (*  Find an entry in an environment, if it exists.  *)
  let findEntry = fun env tsym ->
    let rec find' = fun env ->
      match env with
        EnvironmentCell(sym,entry)::es ->
          if sym = tsym then
            Some(entry)
          else
            find' es
      | [] -> None
    in
    (find' env)
  in
  
  let rec normalize = fun term env whnf ->
    match term with
      Absyn.IntTerm(_)
    | Absyn.RealTerm(_)
    | Absyn.StringTerm(_)
    | Absyn.FreeVarTerm(_)
    | Absyn.ConstantTerm(_) -> (TermEntry(term))
    
    (*  Bound Variables: First check to see if the variable has been
        encountered.  If so, check for an environment entry, and use
        the corresponding term or suspension.  Otherwise, simply use
        the term. *)
    | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), _, pos) ->
        let entryop = (findEntry env tsym) in
        if (Option.isSome entryop) then
          let entry = Option.get entryop in
          if (isEntrySuspension (!entry)) then
            let susp = (!entry) in
            let entry' = normalize (getEntrySuspensionTerm susp) 
                          (getEntrySuspensionEnv susp) whnf in
            (entry := entry';
            entry')
          else
            (!entry)
        else
          (TermEntry(term))
    (*  Application *)
    | Absyn.ApplicationTerm(Absyn.CurriedApplication(l, r), b, p) ->
        let l' = normalize l env true in
        if (isEntrySuspension l') then
          let t' = (getEntrySuspensionTerm l') in
          let sym = (Absyn.getTermAbstractionVar t') in
          let body = (Absyn.getTermAbstractionBody t') in
          let env' = makeSuspension sym r env (getEntrySuspensionEnv l') in
          (normalize body env' whnf)
        else
          let l' = (getEntryTerm l') in
          let r' = (getEntryTerm (normalize r env false)) in
          TermEntry(Absyn.ApplicationTerm(Absyn.CurriedApplication(l', r'), b, p))
    
    | Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, aterm), _, pos) ->
        if whnf then
          SuspensionEntry(term, env)
        else if (getEnvironmentSize env) > 0 then
          let tsym' = Absyn.BoundVar(Absyn.getTypeSymbolSymbol tsym, ref None, ref false, ref(Some(Absyn.getTypeSymbolType tsym))) in
          let t = Absyn.makeBoundVarTerm tsym' pos in
          let t' = makeTerm tsym' t env in
          let aterm' = (getEntryTerm (normalize aterm t' false)) in
          if aterm <> aterm' then
            TermEntry(Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym', aterm'), false, pos))
          else
            TermEntry(term)
        else
          let aterm' = (getEntryTerm (normalize aterm emptyEnvironment false)) in
          if aterm <> aterm' then
            TermEntry(Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, aterm'), false, pos))
          else
            TermEntry(term)
    | Absyn.ErrorTerm -> TermEntry(Absyn.ErrorTerm)
    | _ ->
        (Errormsg.error (Absyn.getTermPos term) "Parse.normalize: invalid term type";
        TermEntry(Absyn.ErrorTerm))
  in

  let result = normalize term emptyEnvironment false in
  (getEntryTerm result)

and fixTerm term =
  let _ = Errormsg.error Errormsg.none "Parse.fixTerm: not implemented" in
  term

let illegalConstant c pos =
  if c == Pervasive.implConstant || c == Pervasive.colondashConstant then
    (Errormsg.error pos ("symbol " ^ (Absyn.getConstantName c) ^
      " cannot be embedded in predicate arguments");
    true)
  else
    false

let unitTests () =
  let absyn = Absyn.Module("UnitTests", [], [],
    ref (Pervasive.pervasiveConstants), ref (Pervasive.pervasiveKinds), Table.empty,
    [], [], [], [], [], ref [], [], ref [], ref (Absyn.ClauseBlocks([])))
  in

  let test t =
    let t' = translateTerm t absyn in
    let _ = Errormsg.log Errormsg.none ("Preabsyn Term: " ^ (Preabsyn.string_of_term t)) in
    let _ = Errormsg.log Errormsg.none ("Absyn Term: " ^ (Absyn.string_of_term t')) in
    ()
  in
  
  let _ = Errormsg.log Errormsg.none ("Parse Unit Tests:") in
    
  let t1 = Preabsyn.SeqTerm([Preabsyn.IdTerm(Symbol.symbol "true", None, Preabsyn.ConstID, Errormsg.none)], Errormsg.none) in
  let t2 = Preabsyn.SeqTerm([Preabsyn.IntTerm(1, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol "+", None, Preabsyn.ConstID, Errormsg.none); Preabsyn.IntTerm(1, Errormsg.none)], Errormsg.none) in
  let t3 = 
    Preabsyn.SeqTerm([Preabsyn.IntTerm(1, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol "+", None, Preabsyn.ConstID, Errormsg.none); t2], Errormsg.none) in  

  let t5 = Preabsyn.ListTerm([Preabsyn.IntTerm(1, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol ",", None, Preabsyn.ConstID, Errormsg.none); Preabsyn.IntTerm(2, Errormsg.none)], Errormsg.none) in
  let t6 = Preabsyn.SeqTerm([Preabsyn.IntTerm(1, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol "::", None, Preabsyn.ConstID, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol "nil", None, Preabsyn.ConstID, Errormsg.none)], Errormsg.none) in
  let t7 = Preabsyn.SeqTerm([Preabsyn.IntTerm(1, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol "::", None, Preabsyn.ConstID, Errormsg.none); Preabsyn.IntTerm(2, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol "::", None, Preabsyn.ConstID, Errormsg.none); Preabsyn.IdTerm(Symbol.symbol "nil", None, Preabsyn.ConstID, Errormsg.none)], Errormsg.none) in

  let () = test t1 in
  let () = test t2 in
  let () = test t3 in
  let () = test t5 in
  let () = test t6 in
  let () = test t7 in
  
  let _ = Errormsg.log Errormsg.none ("Parse Unit Tests Compete.\n") in
  ()
