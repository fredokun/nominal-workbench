(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Vincent Botbol
*)

(** Configuration variables *)

open Arg

(** Current version *)
val version : string

(** Path variable *)
val get_path : unit -> string list

(** Suffix to rules files *)
val rule_suffix : string

(* If true, do not launch the REPL after having evaluated the files. *)
(** Is the toplevel enabled *)
val no_repl : bool ref

(* If true, the warnings will be silently ignored and not printed. *)
val no_warning : bool ref

(* If True, the mode debug is activated. *)
(** Is debug mode enabled *)
val debug : bool ref

(** Is verbose mode enabled *)
val verbose : bool ref

(** Use a clean system after loading the files *)
val reset_system : bool ref

(** Arguments list *)
val list : (key * spec * doc) list
