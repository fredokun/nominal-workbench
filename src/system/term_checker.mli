(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Term_ast
open Term_ast_dag
open Symbols
open Rewriting_ast

val construct_ast_checked : system -> term_ast -> term_dag
val check_type_of_term : system -> term_dag -> term_type
val type_of_typed_term : term_type -> type_application
