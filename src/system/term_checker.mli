(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Term_ast
open Term_ast_typed
open Symbols
open Rewriting_ast

val construct_ast_checked : system -> term_ast -> term_ast_with_binders
val check_type_of_term : system -> term_ast_with_binders -> term_ast_typed
val type_of_typed_term : term_ast_typed -> type_application
