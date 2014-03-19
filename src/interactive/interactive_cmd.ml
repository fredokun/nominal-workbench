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
  | Failed of error * string (* error message *)

type rewriting_test = RewritingTest of filename * expectation

let string_of_error (Error(domain, name)) =
  sprintf "%s.%s" domain name

let equal_error (Error(domain1, name1)) (Error(domain2, name2)) =
  (name1 = name2) && (domain1 = domain2)

let strip_ws str =
  Str.global_replace (Str.regexp " +") "" str

let string_of_filename filename =
  sprintf "    Filename: %s\n" filename

let string_of_msg msg =
  sprintf "    Message: %s\n" msg

let string_of_test_info filename msg =
  string_of_filename filename ^ string_of_msg msg

(* Only a textual equality test. *)
let equal_term t1 t2 =
  (strip_ws t1) = (strip_ws t2)

let string_of_terms ts =
    List.sort String.compare @@ List.map Pretty.(string_of pp_term) ts

let flatten_string_of_terms ts =
  String.concat "; " @@ string_of_terms ts

let equal_terms t1s t2s =
  let t1s_str = string_of_terms t1s in
  let t2s_str = string_of_terms t2s in
  try
    List.for_all2 equal_term t1s_str t2s_str
  with
  | Invalid_argument _ -> false

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

(* Lot of redundant code, should be factorized, we probably need functor for this. *)
let check_term eval_term = function
  | TMustPass (InPredicate(t1, t2))
  | TMustPass (EqualPredicate(t1, t2)) ->
  begin
    try
      let rt1s = eval_term t1 in
      let rt2s = List.flatten @@ List.map eval_term t2 in
      if (equal_terms rt1s rt2s) then
        rewritten_success rt1s rt2s
      else
        rewritten_failure_unexpected rt1s rt2s
    with
    | Rewriting_error.RewritingError(code, msg) -> Rewriting_error.(
        rewritten_failure (string_of_error_code code)
          (string_of_msg (error_msg code msg)))
    | Term_system_error.TermSystemError(code, msg) -> Term_system_error.(
        rewritten_failure (string_of_error_code code)
            (string_of_msg (error_msg code msg)))
    | e -> print_unknown_exc e "term rewriting"
  end
  | TMustFail (term, e) ->
    try
      let rt = flatten_string_of_terms @@ eval_term term in
      print_failure (
        sprintf "Should have failed with %s but passed with %s.\n"
          (string_of_error e) rt)
    with
    | Rewriting_error.RewritingError(code, msg) -> Rewriting_error.(
      if equal_error e (Error(domain_name, string_of_error_code code)) then
        print_success (
          sprintf "Failure with %s as expected.\n" (string_of_error e) ^
          string_of_msg @@ error_msg code msg)
      else
        print_failure (
          sprintf "Expected error %s but failed with %s.\n"
            (string_of_error e)
            (string_of_error (Error(domain_name, string_of_error_code code))) ^
          string_of_msg @@ error_msg code msg))
    | Term_system_error.TermSystemError(code, msg) -> Term_system_error.(
      if equal_error e (Error(domain_name, string_of_error_code code)) then
        print_success (
          sprintf "Failure with %s as expected.\n" (string_of_error e) ^
          string_of_msg @@ error_msg code msg)
      else
        print_failure (
          sprintf "Expected error %s but failed with %s.\n"
            (string_of_error e)
            (string_of_error (Error(domain_name, string_of_error_code code))) ^
          string_of_msg @@ error_msg code msg))
    | e -> print_unknown_exc e "term rewriting"


let system_name filename = (Filename.chop_extension (Filename.basename filename))

(* Rewriting System test *)
let check_expectation filename expectation result domain =
  match expectation, result with
  | MustPass, Failed(e, msg) ->
    print_failure (
      sprintf "Failure with error %s.\n" (string_of_error e) ^
      string_of_test_info filename msg)
  | MustFail(e), Passed ->
    print_failure (
      sprintf "Should have failed with %s.\n" (string_of_error e) ^
      string_of_filename filename)
  | MustFail(expected), Failed(e, msg) when not ( equal_error expected e ) ->
      print_failure (sprintf "Expected error %s but failed with %s.\n"
        (string_of_error expected)
        (string_of_error e) ^
        string_of_test_info filename msg)
  | MustFail(_), Failed(e, msg) ->
      print_success (
        sprintf "Failure with %s as expected.\n" (string_of_error e) ^
        string_of_test_info filename msg)
  | MustPass, Passed ->
      print_success (
        sprintf "%s passed.\n" (system_name filename) ^
        string_of_filename filename)

let check_rewriting_system eval_ast ast (RewritingTest(filename, expectation)) =
  let open Rewriting_system_error in
  try
    ignore (eval_ast ast);
    check_expectation filename expectation Passed domain_name
  with
  | RewritingSystemError(code, msg) ->
    check_expectation filename expectation
      (Failed(Error(domain_name, string_of_error_code code), error_msg code msg)) domain_name
  | e -> print_unknown_exc e (sprintf "check of the rewriting system (%s)" filename)

let test_rewriting_system eval_ast channel (RewritingTest(filename, expectation) as test) =
  let open Rewriting_parsing_error in
  try
    check_rewriting_system eval_ast (Util.parse_channel channel) test
  with
  | RewritingParsingError(code, msg) ->
      check_expectation filename expectation
        (Failed(Error(domain_name, string_of_error_code code), error_msg code msg)) domain_name
  | e -> print_unknown_exc e (sprintf "parsing of the rewriting system (%s)" filename)

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
    | e -> print_unknown_exc e (sprintf "launching of the test (%s)" filename) in
  Printexc.record_backtrace true;
  launch ();
  Printexc.record_backtrace false

let match_term eval_term term_expr pattern system =
  try
    ignore (Type_checking.check_pattern system pattern);
    List.iter (fun term ->
        let res =
          match Matching.matching
                  (Term_checker.construct_ast_checked system term)
                  pattern with
          | Some _ -> "The term matches the pattern."
          | None -> "The term doesn't match the pattern." in
        print_endline res) @@ eval_term term_expr
  with Rewriting_system_error.RewritingSystemError _ ->
    print_system_error "Pattern ill-formed\n"

let print_type_of_term eval_term term_expr system =
  let open Term_checker in
  List.iter (fun term -> print_endline
    @@ Pretty.(string_of pp_type_application)
    @@ type_of_typed_term
    @@ check_type_of_term system
    @@ construct_ast_checked system term)
    (eval_term term_expr)

let term_match_type eval_term term_expr system type_binders arg_types =
  if List.length arg_types <> 1 then
    print_failure "Give one type please"
  else
    let arg_type = List.hd arg_types in
    let open Term_checker in
    let open Term_system_error in
    let gen_binders = binders_to_TBinds type_binders in
    List.iter
      (fun term ->
        let term_checked = construct_ast_checked system term in
        try
          begin
            ignore (unify_term_and_type system gen_binders term_checked arg_type);
            print_success (sprintf "The term %s match the type %s\n"
                             (Pretty.(string_of pp_term term))
                             (Pretty.(string_of pp_operator_arg arg_type)))
          end
        with
        | TermSystemError (code, info) ->
          print_failure (sprintf "The term %s does not match the type %s with error :\n%s\n"
                           (Pretty.(string_of pp_term term))
                           (Pretty.(string_of pp_operator_arg arg_type))
                           (error_msg code info))
        | e -> print_unknown_exc e (sprintf "type checking of the term %s\n"
                                      (Pretty.(string_of pp_term term))))
      (eval_term term_expr)

let eval_interactive_cmd process_term_expr eval_system system = function
| LoadTest(filename, expectation) ->
  launch_test (eval_system system) (RewritingTest(filename, expectation));
  system
| TermTest(term) -> check_term (process_term_expr system) term; system
| TermMatch(term, pattern) ->
  match_term (process_term_expr system) term pattern system;
  system
| TermType(term_expr) ->
  print_type_of_term (process_term_expr system) term_expr system;
  system
| TermMatchType (term_expr, type_binders, arg_types) ->
  term_match_type (process_term_expr system) term_expr system type_binders arg_types;
  system
| Quit -> exit 0
