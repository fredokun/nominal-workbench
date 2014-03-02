(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Term_ast
open Term_ast_typed
open Symbols
open Rewriting_ast

val binders_to_TBinds : type_binders -> bnd_typ_app TBinders_map.t

val construct_ast_checked : system -> term_ast -> term_ast_with_binders

val check_type_of_term : system -> term_ast_with_binders -> term_ast_typed

val unify_term_and_type :
  system -> bnd_typ_app TBinders_map.t -> term_ast_with_binders -> operator_arg -> term_ast_typed

val type_of_typed_term : term_ast_typed -> type_application
