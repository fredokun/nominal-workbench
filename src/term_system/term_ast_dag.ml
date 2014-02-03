(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
  (C) Copyright Pierrick Couderc
  (C) Copyright Matthieu Dien
*)

type info = Lexing.position

type ident = string

type term_dag =
  | DConst of ident
  | DTerm of ident * term_dag list
  (* Because when you find a binder, you don't know its sons *)
  | DBinder of ident * (((term_dag ref) list) ref)
  | DVar of ident * ((term_dag ref) option)

type term_ast_dag = TermAstDag of (info * term_dag) list

(*
let mk_node e t = { value = e; info = t}
let mk_dummy e = { value = e; info = ()}
*)

let rec string_of_term : term_dag -> string = function
  | DConst id -> id
  | DVar (id,_) -> "$" ^ id
  | DBinder (id,_) -> "l" ^ id
  | DTerm (name, expr_l) ->
    name ^ "(" ^ String.concat ", " (List.map string_of_term expr_l) ^ ")"
