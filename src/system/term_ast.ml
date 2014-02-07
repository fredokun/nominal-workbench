(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
  (C) Copyright Pierrick Couderc
  (C) Copyright Matthieu Dien
*)

type info = Lexing.position

type ident = string

type term =
  | Const of ident
  | Term of ident * term list
  | Var of ident

type term_ast = TermAst of (info * term) list

(*
let mk_node e t = { value = e; info = t}
let mk_dummy e = { value = e; info = ()}
*)

let rec string_of_term : term -> string = function
  | Const id -> id
  | Var (id) -> "$" ^ id
  | Term (name, expr_l) ->
    name ^ "(" ^ String.concat ", " (List.map string_of_term expr_l) ^ ")"
