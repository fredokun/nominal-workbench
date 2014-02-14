(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Printf
open Display_test
open Parsing_ast

(* Test framework. *)
type result =
  | Passed
  | Failed of error

type rewriting_test = RewritingTest of filename * expectation

let string_of_error (Error(domain, name)) =
  sprintf "%s.%s" domain name

let equal_error (Error(domain1, name1)) (Error(domain2, name2)) =
  (name1 = name2) && (domain1 = domain2)

let strip_ws str =
  Str.global_replace (Str.regexp " +") "" str

(* Only a textual equality test. *)
let equal_term t1 t2 =
  (strip_ws t1) = (strip_ws t2)

let rewritten_success t1 t2 =
  print_success (sprintf "Term %s has been correctly rewritten in %s." t1 t2)

let check_term eval_term = function
  | TMustPass (InPredicate(t1, t2))
  | TMustPass (EqualPredicate(t1, t2)) ->
    let rt1 = Term_ast.string_of_term @@ eval_term t1 in
    let rt2 = Term_ast.string_of_term @@ eval_term t2 in
    if (equal_term rt1 rt2) then
      rewritten_success rt1 rt2 (* FIXME : display twice the same thing *)
    else
      print_failure (sprintf "Bad term rewriting, expected %s but got %s." rt2 rt1)
  | TMustFail (term, e) ->
    let open Rewriting_error in
    try
      let rt = Term_ast.string_of_term @@ eval_term term in
      print_failure (sprintf "Should have failed with %s but passed with %s." (string_of_error e) rt)
    with
    | RewritingError(code, _) when equal_error e (Error(domain_name, string_of_error_code code)) ->
        print_success (sprintf "Failure with %s as expected." (string_of_error e))
    | RewritingError(code, _) ->
        print_failure (sprintf "Expected error %s but failed with %s."
          (string_of_error e)
          (string_of_error (Error(domain_name, string_of_error_code code))))
    | e -> print_unknown_exc e "term rewriting"

(* Rewriting System test *)
let check_expectation expectation result domain success_cont =
  match (expectation, result) with
  | (MustPass, Failed(e)) -> print_failure (sprintf "Failure with error %s." (string_of_error e))
  | (MustFail(e), Passed) -> print_failure (sprintf "Should have failed with %s." (string_of_error e))
  | (MustFail(expected), Failed(e)) when not ( equal_error expected e ) ->
      print_failure (sprintf "Expected error %s but failed with %s."
        (string_of_error expected)
        (string_of_error e))
  | (MustFail(_), Failed(e)) ->
      print_success (sprintf "Failure with %s as expected." (string_of_error e))
  | (MustPass, Passed) -> success_cont ()

let system_name filename = (Filename.chop_extension (Filename.basename filename))

let check_rewriting_system eval_ast ast (RewritingTest(filename, expectation)) =
  let open Rewriting_system_error in
  try 
    let success_cont () = print_success (sprintf "%s passed." (system_name filename)) in
    ignore (eval_ast ast);
    check_expectation expectation Passed domain_name success_cont
  with
  | RewritingSystemError(code, _) ->
    check_expectation expectation (Failed(Error(domain_name, string_of_error_code code)))
      domain_name ignore
  | e -> print_unknown_exc e "check of the rewriting system"

let test_rewriting_system eval_ast channel (RewritingTest(_, expectation) as test) =
  let open Rewriting_parsing_error in
  try
    check_rewriting_system eval_ast (Parser_include.parse_nowork_file channel) test
  with
  | RewritingParsingError(code,_) ->
      check_expectation expectation (Failed(Error(domain_name, string_of_error_code code)))
        domain_name ignore
  | e -> print_unknown_exc e "parsing of the rewriting system"

let launch_test eval_ast (RewritingTest(filename, _) as test) =
  let launch () = 
    (* print_test no name file; *)
    try
      if Sys.is_directory filename then
        print_system_error (sprintf "%s: is a directory" filename)
      else
        let f = open_in filename in
        test_rewriting_system eval_ast f test;
        close_in f
    with
    | Sys_error(e) -> print_system_error e
    | e -> print_unknown_exc e "launching of the test" in
  Printexc.record_backtrace true;
  launch ();
  Printexc.record_backtrace false

let eval_interactive_cmd process_term_expr eval_system system = function
| LoadTest(filename, expectation) -> 
  launch_test (eval_system system) (RewritingTest(filename, expectation));
  system
| TermTest(term) -> check_term (process_term_expr system) term; system
| Quit -> exit 0
