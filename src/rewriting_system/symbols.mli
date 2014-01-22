(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast
open Lexing

(* Add a symbol in the global environment. Warn on symbol redeclaration. *)
val add_symbol : string * info * rewriting_declaration -> unit
(* Add a symbol in the global environment. Raise a RewritingSystemError on redeclaration. *)
val add_symbol_strict : string * info * rewriting_declaration -> unit

(* Set the global environment with the declarations of the AST.
  Warn on symbol redeclaration. *)
val set_up_environment : rewriting_ast -> unit

(* Set the global environment with the declarations of the AST.
  Raise a RewritingSystemError on redeclaration. *)
val set_up_environment_strict : rewriting_ast -> unit

val clear_symbols : unit -> unit

val lookup_kind : ?pos:position -> string -> info * kind
val lookup_const : ?pos:position -> string -> info * constant
val lookup_op : ?pos:position -> string -> info * operator
val lookup_rule : ?pos:position -> string -> info * rule

val is_kind : string -> bool
val is_const : string -> bool
val is_op : string -> bool
val is_rule : string -> bool

(* tmp *)
val list_of_rules : unit -> Rewriting_ast.rule list
