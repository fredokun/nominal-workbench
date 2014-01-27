(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
*)

type info = Lexing.position

type ident = string

type term =
  | Const of ident
  (* Comment on représente une liste de binder? *)
  | Abstraction of ident * ident * term list
  | Call of ident * term list
  | Var of ident * (term ref) option (* DAG arrow *)

type term_ast = TermAST of (info * term) list

(*
let mk_node e t = { value = e; info = t}
let mk_dummy e = { value = e; info = ()}
*)


let rec string_of_term : term -> string = function
  | Const id -> id
  | Var (id, _) -> "$" ^ id
  | Abstraction (name, plh, expr_l) ->
    name ^ "([" ^ plh ^ "], "^
      String.concat ", " (List.map string_of_term expr_l) ^ ")"
  | Call (name, expr_l) ->
    name ^ "(" ^ String.concat ", " (List.map string_of_term expr_l) ^ ")"
