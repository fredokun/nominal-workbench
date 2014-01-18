(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast

val add : string * info * rewriting_declaration -> unit

val lookup_kind : string -> info * kind
val lookup_const : string -> info * constant
val lookup_op : string -> info * operator
val lookup_rule : string -> info * rule
