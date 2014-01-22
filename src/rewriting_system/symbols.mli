(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast

(* Add a symbol in the global environment. Warn on symbol redeclaration. *)
val add_symbol : string * info * rewriting_declaration -> unit
(* Add a symbol in the global environment. Raise a RewritingSystemError on redeclaration. *)
val add_symbol_strict : string * info * rewriting_declaration -> unit

val lookup_kind : string -> info * kind
val lookup_const : string -> info * constant
val lookup_op : string -> info * operator
val lookup_rule : string -> info * rule

val is_kind : string -> bool
val is_const : string -> bool
val is_op : string -> bool
val is_rule : string -> bool
