(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Printf

let ncolumns = 140

let red_color = "\x1b[0;31m"
let green_color = "\x1b[0;32m"
let cyan_color = "\x1b[0;36m"
let reset_color = "\x1b[0m"

let make_spaces n =
  let res = String.create n in
  String.fill res 0 (String.length res) ' ';
  res

let print_result_line leftline color rightline =
  let padding = ncolumns - String.length leftline - String.length rightline in
  let spaces = make_spaces padding in
  printf "%s%s%s%s%s\n" leftline spaces color rightline reset_color

let print_system_error s =
  print_result_line s red_color "[ system error ]"

let print_failure s =
  print_result_line s red_color "[ failed ]"

let print_success s =
  print_result_line s green_color "[ passed ]"

let print_test no name file =
  printf "%sTest %d%s %s (%s).\n" cyan_color no reset_color name file

let print_unknown_exc e action_name = print_failure (sprintf 
  "Unexpected exception (%s) caught during the %s."
      (Printexc.to_string e) action_name)
