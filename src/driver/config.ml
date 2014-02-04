(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Vincent Botbol
*)

let version = "0.00.1"

let path = ref ["."]

let get_path = !path

let rule_suffix = "nw"

let term_suffix = "nwt"

let verbose = ref false

(* functions *)
let print_version () =
  Printf.printf "NoWork toplevel, version %s\n" version;
  exit 0

let print_vnum () =
  Printf.printf "%s\n" version;
  exit 0

let add_path p =
  path := p :: !path

(* module Arg list *)
let list = 
  let open Arg in
      [ "--version", Unit print_version, "Print version and exits"
      ; "--vnum", Unit print_vnum, "Print version number and exits"
      ; "-I", String add_path, "Include the given directory"
      ; "-v", Set verbose, "Be more verbose"
      ]

