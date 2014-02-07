(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Vincent Botbol
*)

(** Toplevel entry point *)

(** Process the arguments and call the continuation with the resulting
    system *)
val main : (Symbols.system -> unit) -> unit

(** Evaluates a file with a given system and returns the newly formed
    one *)
val process_file : Symbols.system -> string -> Symbols.system
