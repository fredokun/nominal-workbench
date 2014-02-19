(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Printf

let red_color = "\x1b[0;31m"
let green_color = "\x1b[0;32m"
let cyan_color = "\x1b[0;36m"
let reset_color = "\x1b[0m"

let print_result_line message color status =
  printf "%s%s%s  %s" color status reset_color message

let print_system_error s =
  print_result_line s red_color "[ system error ]"

let print_failure s =
  print_result_line s red_color "[ failed ]"

let print_success s =
  print_result_line s green_color "[ passed ]"

let print_test no name file =
  printf "%sTest %d%s %s (%s).\n" cyan_color no reset_color name file

let print_exc_backtrace () =
  let open Str in
  let print_backtrace_line l = Printf.printf "  %s\n" l in
  List.iter print_backtrace_line (split (regexp "[\n|\r|\r\n]") (Printexc.get_backtrace ()))

let print_unknown_exc e action_name = print_failure (sprintf 
  "Unexpected exception (%s) caught during the %s. Backtrace:"
      (Printexc.to_string e) action_name);
  print_exc_backtrace ()
