(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast
open Lexing

val enter_ast : rewriting_ast -> unit

val clear_symbols : unit -> unit

val lookup_kind : ?pos:position -> string -> info * kind
val lookup_const : ?pos:position -> string -> info * constant
val lookup_op : ?pos:position -> string -> info * operator
val lookup_rule : ?pos:position -> string -> info * rule

val is_kind : string -> bool
val is_const : string -> bool
val is_op : string -> bool
val is_rule : string -> bool
