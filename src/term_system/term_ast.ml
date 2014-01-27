(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
*)

type info = Lexing.position

type ident = string

type type_name = string

type term =
  | Const of ident
  | Binder of ident
  | Term of ident * type_name list * term list
  | Var of ident * (term ref) option (* DAG arrow *)

type term_ast = TermAST of (info * term) list

(*
let mk_node e t = { value = e; info = t}
let mk_dummy e = { value = e; info = ()}
*)

let rec string_of_term : term -> string = function
  | Const id -> id
  | Binder id -> "[" ^ id ^ "]"
  | Var (id, _) -> "$" ^ id
  | Term (name, types_l, expr_l) ->
    name ^ "(" ^ String.concat ", " (List.map string_of_term expr_l) ^ ")"
