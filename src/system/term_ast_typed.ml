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

type term_ast_with_binders =
| DConst of info option * ident
| DTerm of info option * ident * term_ast_with_binders list
| DBinder of info option * ident
| DVar of info option * ident

module TBinders_map = Map.Make(String)

(* Because cyclic sucks *)
type bnd_typ_app = BndTypApp of (bnd_typ_app TBinders_map.t) * type_application

type genericity =
| Gen
| Inst of bnd_typ_app
| Simple

type term_ast_typed =
| TypedConst of bnd_typ_app
| TypedTerm of bnd_typ_app
| TypedBinder of type_application
| TypedVar of type_application

let rec string_of_term : term_ast_with_binders -> string = function
  | DConst (_, id) -> id
  | DVar (_, id) -> "$" ^ id
  | DBinder (_, id) -> "l" ^ id
  | DTerm (_, name, expr_l) ->
    name ^ "(" ^ String.concat ", " (List.map string_of_term expr_l) ^ ")"
