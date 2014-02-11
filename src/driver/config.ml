(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Vincent Botbol
*)

let version = "0.00.1"

let path = ref ["."]

let get_path () = List.rev !path

let rule_suffix = "nw"
let term_suffix = "nwt"
let verbose = ref false
let no_repl = ref false
let reset_system = ref false

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
  ; "--no-repl", Set no_repl, "Read files and then exit without launching the top-level."
  ; "--reset-system", Set reset_system, "Start interpreting each file with an empty system instead of populating it"
  ; "--load-system-file", String (fun _ -> print_endline "'load-system' todo"), "Load the given file as the initial system to used"
  ]

