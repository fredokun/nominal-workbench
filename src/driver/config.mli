(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Vincent Botbol
*)

(** Configuration variables *)

open Arg

val version : string

val get_path : unit -> string list

val rule_suffix : string

(* If True, do not launch the REPL after having evaluated the files. *)
val no_repl : bool ref

(* If True, the mode debug is activated. *)
val debug : bool ref

val verbose : bool ref

val reset_system : bool ref

val list : (key * spec * doc) list
