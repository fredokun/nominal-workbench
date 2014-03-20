(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Term_ast
open Term_ast_typed
open Symbols
open Rewriting_ast

(**
   Generate a map of generic type binders used for unification
*)
val binders_to_TBinds : type_binders -> bnd_typ_app TBinders_map.t

(**
   Transforms the basic ast to another semantically well-formed
*)
val construct_ast_checked : system -> term_ast -> term_ast_with_binders

(**
   According to the given system check if a term is well-typed
*)
val check_type_of_term : system -> term_ast_with_binders -> term_ast_typed

(**
   According to a given system unify a well-formed term with a type
   and return a typed term
*)
val unify_term_and_type :
  system -> bnd_typ_app TBinders_map.t ->
  term_ast_with_binders -> operator_arg -> term_ast_typed

(**
   Given a typed term gives its type
*)
val type_of_typed_term : term_ast_typed -> type_application
