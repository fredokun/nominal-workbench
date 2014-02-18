(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
  (C) Copyright Pierrick Couderc
  (C) Copyright Matthieu Dien
*)

open Rewriting_ast

type info = Lexing.position

type ident = string

type term_dag =
  | DConst of ident
  | DTerm of ident * term_dag list
  | DBinder of ident
  | DVar of ident

(* Typed AST *)

(* type type_name = string *)
(* type type_binders = type_name list *)

(* type type_application = *)
(*   | TypeApplication of type_name * type_application list *)
(*   | TypeName of type_name *)

module TBinders_map = Map.Make(String)

(* Because cyclic sucks *)
type bnd_typ_app = BndTypApp of (bnd_typ_app TBinders_map.t) * type_application

type genericity =
| Gen
| Inst of bnd_typ_app
| Simple

type term_type =
  | TypedConst of bnd_typ_app
  | TypedTerm of bnd_typ_app
  | TypedBinder of type_application
  | TypedVar of type_application

let rec string_of_term : term_dag -> string = function
  | DConst id -> id
  | DVar id -> "$" ^ id
  | DBinder id -> "l" ^ id
  | DTerm (name, expr_l) ->
    name ^ "(" ^ String.concat ", " (List.map string_of_term expr_l) ^ ")"
