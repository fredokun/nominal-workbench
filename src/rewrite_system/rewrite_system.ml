
open Ast
open Term_ast

(*
  exemple :

  App (Lambda(x, ?E), e) => E[e/x]

 
*)


module SMap = Map.Make(String)

type 'info term_effect_raw =
  | TEconstant of 'info ident (* True *)
  | TEplaceholder of 'info ident (* ?x *)
  | TEoperator of 'info ident * 'info term_effect list
and 'info term_effect = ('info term_effect_raw, 'info) annotated

let rewrite pattern effect term =
  let rec substitute placeholders effect =
    match effect with
    | TEconstant ident -> Const ident
    | TEplaceholder ident ->
        begin try SMap.find ident placeholders with Not_found ->
          raise (Error (UnknownPlaceholder ident)) end
    | TEoperator (ident, operands) ->
        Call (ident, List.map (substitute placeholders) operands)
  in 
  let matches, ph = Matching.matching term pattern in
  if matches then substitute ph effect else term

(* match term with *)
  (* | Const _ | Var _ -> term *)
  (* | Abstraction (name, bind, exprs) -> *)
  (*     List.map (rewrite pattern) exprs *)
  (* | Call (name, exprs) -> *)
  
  
  













