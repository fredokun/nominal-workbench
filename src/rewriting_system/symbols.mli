(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast
open Lexing

type system

val empty_system : system

(* Add a symbol in the global environment. Warn on symbol redeclaration. *)
val add_symbol : system -> string * info * rewriting_declaration -> system
(* Add a symbol in the global environment. Raise a RewritingSystemError on redeclaration. *)
val add_symbol_strict : system -> string * info * rewriting_declaration -> system

(* Set the global environment with the declarations of the AST.
  Warn on symbol redeclaration. *)
val enter_ast : system -> rewriting_ast -> system

(* Set the global environment with the declarations of the AST.
  Raise a RewritingSystemError on redeclaration. *)
val enter_ast_strict : system -> rewriting_ast -> system

val lookup_kind : system -> ?pos:position -> string -> info * kind
val lookup_const : system -> ?pos:position -> string -> info * constant
val lookup_op : system -> ?pos:position -> string -> info * operator
val lookup_rule : system -> ?pos:position -> string -> info * rule

val is_kind : system -> string -> bool
val is_const : system -> string -> bool
val is_op : system -> string -> bool
val is_rule : system -> string -> bool

(* tmp *)
val list_of_rules : system -> rule list
