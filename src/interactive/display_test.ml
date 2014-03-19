(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Printf
open Parsing_ast

let red_color = "\x1b[0;31m"
let green_color = "\x1b[0;32m"
let cyan_color = "\x1b[0;36m"
let reset_color = "\x1b[0m"

(* Test framework. *)
type result =
  | Passed
  | Failed of error * string (* error message *)

type rewriting_test = RewritingTest of filename * expectation

let string_of_error (Error(domain, name)) =
  sprintf "%s.%s" domain name

let equal_error (Error(domain1, name1)) (Error(domain2, name2)) =
  (name1 = name2) && (domain1 = domain2)

let strip_ws str =
  Str.global_replace (Str.regexp " +") "" str

let string_of_filename filename =
  sprintf "\n    %sFilename:%s %s\n" cyan_color reset_color filename

let string_of_msg msg =
  sprintf "    %sInfo:%s %s\n" cyan_color reset_color msg

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

let string_of_terms ts =
    List.sort String.compare @@ List.map Pretty.(string_of pp_term) ts

let flatten_string_of_terms ts =
  String.concat "; " @@ string_of_terms ts
let rewritten_success t1 t2 =
  print_success (sprintf "Terms have been correctly rewritten in : %s\n%!"
    (flatten_string_of_terms t1)) (* FIXME : find a way to write the original expr *)

let rewritten_failure_unexpected t1 t2 =
  print_failure (sprintf "Bad term rewriting : %s expected but got : %s\n%!"
    (flatten_string_of_terms t2)
    (flatten_string_of_terms t1))

let rewritten_failure error msg =
  print_failure (sprintf "Failure with %s while expecting to succeed on rewriting.\n"
    error ^ msg)
