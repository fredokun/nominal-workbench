(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast
open Strategy_ast
open Lexing

module System_map : sig
  type key = String.t
  type 'a t = 'a Map.Make(String).t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val choose : 'a t -> key * 'a
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

type system = {
  kinds :  (info * kind) System_map.t;
  constants : (info * constant) System_map.t;
  operators : (info * operator) System_map.t;
  rules :(info * rule) System_map.t;
  strategies : (info * strategy_def) System_map.t;
}

val empty_system : system

(* Add a symbol in the global environment. Warn on symbol redeclaration. *)
val add_symbol : system -> rewriting_decl -> system
(* Add a symbol in the global environment. Raise a RewritingSystemError on redeclaration. *)
val add_symbol_strict : system -> rewriting_decl -> system

(* Set the global environment with the declarations of the AST.
  Warn on symbol redeclaration. *)
val enter_decl : system -> rewriting_decl -> system

(* Set the global environment with the declarations of the AST.
  Raise a RewritingSystemError on redeclaration. *)
val enter_decl_strict : system -> rewriting_decl -> system

val lookup_kind : system -> ?pos:position -> string -> info * kind
val lookup_const : system -> ?pos:position -> string -> info * constant
val lookup_op : system -> ?pos:position -> string -> info * operator
val lookup_rule : system -> ?pos:position -> string -> info * rule

val is_kind : system -> string -> bool
val is_const : system -> string -> bool
val is_op : system -> string -> bool
val is_rule : system -> string -> bool
