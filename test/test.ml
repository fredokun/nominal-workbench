(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Printf
open Display_test

type filename = string
type name = string
type domain = string
type error = Error of name * domain

type expected_term = string

type term_expectation =
  | TMustPass of expected_term
  | TMustFail of error

type term_lib = string
type term_test = TermTest of term_lib list * string * term_expectation

type expectation =
  | MustPass
  | MustFail of error

type system_test = SystemTest of name * filename * expectation * term_test list

type test = Test of system_test list

type result =
  | Passed
  | Failed of error

type term_result =
  | TPassed of string
  | TFailed of error

(* Transform XML data to test type. *)
let filter_children xdata name =
  let foreach_child ldata child =
    if name = (Xml.tag child)
    then child :: ldata
    else ldata in
  Xml.fold foreach_child [] xdata

let first_child xdata =
  (List.hd (Xml.children xdata))

let first_child_node xdata name =
  first_child (List.hd (filter_children xdata name))

let child_data xdata name =
  Xml.pcdata (first_child_node xdata name)

let error_of_xml xdata =
  Error((child_data xdata "name"), (child_data xdata "domain"))

let term_expectation_of_xml xdata =
  try
    TMustFail(error_of_xml (first_child_node xdata "error"))
  with
  | Failure(_) -> TMustPass(child_data xdata "result")

let lib_test_of_xml libs =
  List.map (fun xlib -> Xml.pcdata xlib) libs

let term_test_of_xml xdata =
  TermTest(lib_test_of_xml (filter_children xdata "lib"),
    child_data xdata "term",
    term_expectation_of_xml xdata)

let rec term_tests_of_xml = function
  | [] -> []
  | hd::tl -> (term_test_of_xml hd) :: (term_tests_of_xml tl)

let expectation_of_xml xtest =
  try MustFail(error_of_xml (first_child_node xtest "error"))
  with _ -> MustPass

let system_test_of_xml xtest =
  SystemTest(child_data xtest "name", child_data xtest "file",
    expectation_of_xml xtest,
    term_tests_of_xml (filter_children xtest "termtest"))

let test_of_xml xtest =
  Xml.map system_test_of_xml xtest

(* Test framework. *)
let string_of_error (Error(name, domain)) =
  sprintf "%s.%s" domain name

let equal_error (Error(name1, domain1)) (Error(name2, domain2)) =
  (name1 == name2) && (domain1 == domain2)

(* Term rewriting tests. *)
let check_term_expectation expectation result domain success_cont =
  match (expectation, result) with
  | (TMustPass(_), TFailed(e)) ->
    print_failure (sprintf "Failure with error %s." @@ string_of_error e)
  | (TMustFail(e), TPassed(t)) -> print_failure
      (sprintf "Should have failed with %s but passed with %s." (string_of_error e) t)
  | (TMustFail(expected), TFailed(e)) when not @@ equal_error expected e ->
      print_failure (sprintf "Expected error %s but failed with %s."
        (string_of_error expected)
        (string_of_error e))
  | (TMustFail(_), TFailed(e)) ->
      print_success (sprintf "Failure with %s as expected." (string_of_error e))
  | (TMustPass(t1), TPassed(t2)) when t1 <> t2 ->
      print_failure (sprintf "Bad term rewriting, expected %s but got %s." t1 t2)
  | (TMustPass(t1), TPassed(t2)) -> success_cont ()

let rewritten_success t1 t2 () =
  print_success (sprintf "Term %s correctly rewrote in %s." t1 t2)

let check_processed_term term rules expectation =
  let open Rewriting_error in
  try
    let rewritten_term = Rewriting.rewrite rules term in
    let srewritten_term = Term_ast.string_of_term rewritten_term in
    check_term_expectation expectation (TPassed(srewritten_term)) domain_name
     (rewritten_success (Term_ast.string_of_term term) srewritten_term)
  with
  | RewritingError(code, _) ->
      check_term_expectation expectation (TFailed(Error(string_of_error_code code, domain_name)))
        domain_name ignore
  | e -> print_unknown_exc e "term rewriting"

let load_library term_libs =
  List.iter (fun lib -> failwith "term library not yet implemented") term_libs

let check_term (TermTest(libs, term, expectation)) =
  let open Term_parsing_error in
  try
    load_library libs;
    let (Term_ast.TermAST terms) =
      Term_parser.start Term_lexer.token (Lexing.from_string term) in
    if (List.length terms) <> 1 then
      print_system_error "You can only test one term at a time."
    else
      let rules = Symbols.list_of_rules () in
      check_processed_term (snd (List.hd terms)) rules expectation
  with
  | TermParsingError(code, _) ->
    check_term_expectation expectation (TFailed(Error(string_of_error_code code, domain_name)))
      domain_name ignore
  | e -> print_unknown_exc e "parsing of the terms"

let check_terms terms () =
  List.iter check_term terms

(* Rewriting System test *)
let check_expectation expectation result domain success_cont =
  match (expectation, result) with
  | (MustPass, Failed(e)) -> print_failure (sprintf "Failure with error %s." (string_of_error e))
  | (MustFail(e), Passed) -> print_failure (sprintf "Should have failed with %s." (string_of_error e))
  | (MustFail(expected), Failed(e)) when not @@ equal_error expected e ->
      print_failure (sprintf "Expected error %s but failed with %s."
        (string_of_error expected)
        (string_of_error e))
  | (MustFail(_), Failed(e)) ->
      print_success (sprintf "Failure with %s as expected." (string_of_error e))
  | (MustPass, Passed) -> success_cont ()

let check_rewriting_system ast (SystemTest(name, file, expectation, terms)) =
  let open Rewriting_system_error in
  try
    Type_checking.check_ast ast;
    check_expectation expectation Passed domain_name (check_terms terms)
  with
  | RewritingSystemError(code, _) ->
      check_expectation expectation (Failed(Error(string_of_error_code code, domain_name)))
        domain_name ignore
  | e -> print_unknown_exc e "check of the rewriting system"

let test_rewriting_system channel (SystemTest(_,_, expectation, _) as sys_test)  =
  let open Rewriting_parsing_error in
  try
    check_rewriting_system (Parser_include.parse_rewriting_system channel) sys_test
  with
  | RewritingParsingError(code,_) ->
      check_expectation expectation (Failed(Error(string_of_error_code code, domain_name)))
        domain_name ignore
  | e -> print_unknown_exc e "parsing of the rewriting system"

let launch_test no (SystemTest(name, file, _, _) as sys_test) =
  print_test no name file;
  try
    if Sys.is_directory file then
      print_system_error (sprintf "%s: is a directory" file)
    else
      let f = open_in file in
      test_rewriting_system f sys_test;
      close_in f
  with
  | Sys_error(e) -> print_system_error e

let rec launch_tests_impl no tests =
  match tests with
  | [] -> ()
  | hd::tl ->
    launch_test no hd;
    launch_tests_impl (no+1) tl

let launch_tests tests =
  launch_tests_impl 1 tests

let () =
try
  Printexc.record_backtrace true;
  let xtest = Xml.parse_file "data/test/test.xml" in
  let dtd = Dtd.parse_file "data/test/test.dtd" in
  let valid_xtest = Dtd.prove (Dtd.check dtd) "test" xtest in
  launch_tests (test_of_xml valid_xtest)
with
| Dtd.Check_error(e) -> print_endline ("Dtd.Check_error: " ^ (Dtd.check_error e))
| Dtd.Prove_error(e) -> print_endline ("Dtd.Prove_error: " ^ (Dtd.prove_error e))
| Dtd.Parse_error(e) -> print_endline ("Dtd.Parse_error: " ^ (Dtd.parse_error e))
| Xml.Error(e) -> print_endline ("Xml.Error: " ^ (Xml.error e))
| Xml.File_not_found(s) -> printf "File %s not found.\n" s
| e ->
    printf "Something got really wrong, here the exception backtrace:\n";
    printf "Exception: %s.\n" @@ Printexc.to_string e;
    Printexc.print_backtrace stdout
