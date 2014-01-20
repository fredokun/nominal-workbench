
open Rewriting_ast
open Term_ast

(*
  exemple :

  App (Lambda(x, ?E), e) => E[e/x]

 
*)

type error = UnknownPlaceholder of string
exception Error of error

let rewrite pattern effect term =
  let rec substitute placeholders effect =
    match effect with
    | EConstant ident -> mk_dummy (Const ident)
    | EPlaceholder ident ->
        begin try Matching.SMap.find ident placeholders
          with Not_found ->
            raise (Error (UnknownPlaceholder ident)) end
    | EOperator (ident, operands) ->
        mk_dummy
          (Call (ident, List.map (substitute placeholders) operands))
  in
  let matches, ph = Matching.matching term pattern in
  if matches then substitute ph effect else term

(* match term with *)
  (* | Const _ | Var _ -> term *)
  (* | Abstraction (name, bind, exprs) -> *)
  (*     List.map (rewrite pattern) exprs *)
  (* | Call (name, exprs) -> *)
  
  
  













