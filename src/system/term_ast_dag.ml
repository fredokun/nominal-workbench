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
  | DBinder of ident
  | DVar of ident

(* Typed AST *)

type type_name = string
type type_binders = type_name list

type type_application =
  | TypeApplication of type_name * type_application list
  | TypeName of type_name

type term_type =
  | TypedConst of type_application
  | TypedTerm of type_application list * type_application
  | TypedBinder of type_name
  | TypedVar of type_name

let rec string_of_term : term_dag -> string = function
  | DConst id -> id
  | DVar id -> "$" ^ id
  | DBinder id -> "l" ^ id
  | DTerm (name, expr_l) ->
    name ^ "(" ^ String.concat ", " (List.map string_of_term expr_l) ^ ")"
