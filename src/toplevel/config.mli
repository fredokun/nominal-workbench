(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Vincent Botbol
*)

(** Configuration variables *)

open Arg

val version : string

val path : string list ref

val rule_suffix : string

val term_suffix : string

val verbose : bool ref

val list : (key * spec * doc) list
