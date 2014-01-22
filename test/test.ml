(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Printf
open Display_test
open Parser_include
open Symbols
open Type_checking

type term_result = string
type term_lib = string
type term_test = TermTest of term_lib list * string * term_result

type domain = string
type name = string
type error = Error of name * domain

type expectation =
  | MustPass of term_test list
  | MustFail of Error

type system_test = SystemTest of string * string * expectation

type test = Test of system_test list

type result =
  | Passed
  | Failed of string

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

let term_test_of_xml xdata =
  TermTest((child_data xdata "term"), (child_data xdata "result"))

let rec term_tests_of_xml = function
  | [] -> []
  | hd::tl -> (term_test_of_xml (first_child_node hd "term")) :: (term_tests_of_xml tl)

let error_of_xml xdata =
  Error((child_data xdata "name"), (child_data xdata "domain"))

let expectation_of_xml xtest =
  try MustFail(error_of_xml (first_child_node xtest "error"))
  with Failure(_) -> MustPass(term_tests_of_xml (filter_children xtest "termtest"))

let system_test_of_xml xtest =
  SystemTest(child_data xtest "name", child_data xtest "file", expectation_of_xml xtest)

let test_of_xml xtest =
  Xml.map system_test_of_xml xtest

(* Test framework. *)

let check_expectation expectation result =
  match (expectation, result) with
  | (MustPass(_), Failed(s)) -> print_failure (sprintf "Failure with error %s." s)
  | (MustFail(s), Passed) -> print_failure (sprintf "Should have failed with %s." s)
  | (MustFail(expected), Failed(s)) when expected <> s ->
      print_failure (sprintf "Expected error %s but fails with %s." expected s)
  | (MustFail(expected), Failed(s)) ->
      print_success (sprintf "Failure with %s as expected." s)
  | (MustPass(terms), Passed) -> print_success "Successfully built the term system."

let check_rewriting_system ast (SystemTest(name, file, expectation)) = 
  try
    Type_checking.check_ast ast;
  with
  | RewritingSystemError(e) -> failwith "Unknown"

let parse_rewriting_system channel expectation =
  try
    check_rewriting_system (Parser_include.parse_channel channel)
  with
  | Parsing.Parse_error -> failwith "not implemented."
  | e -> print_failure (sprintf 
      "Unexpected exception (%s) caught during the parsing of the rewriting system."
      Printexc.to_string e)

let test_expectation channel expectation =
  let open Term_system_error in
  (* let open Term_system_error_code in *)
  let match_result_expectation result =
    match (expectation, result) with
    | (MustPass(_), Failed(s)) -> print_failure (sprintf "Failure with error %s." s)
    | (MustFail(s), Passed) -> print_failure (sprintf "Should have failed with %s." s)
    | (MustFail(expected), Failed(s)) when expected <> s ->
        print_failure (sprintf "Expected error %s but fails with %s." expected s)
    | (MustFail(expected), Failed(s)) ->
        print_success (sprintf "Failure with %s as expected." s)
    | (MustPass(terms), Passed) -> print_success "Successfully built the term system." in
  try
    let ast_list = parse_channel channel in
    List.iter (fun ast ->
      set_up_environment_strict ast;
      ast_well_formed ast;
      check_ast ast;
      clear_symbols ()) ast_list;
    match_result_expectation Passed
  with
  | TermSystemError(e, _) ->
    match_result_expectation (Failed(string_of_error_code e))
  | e ->
    print_failure (sprintf "Unexpected exception: %s" (Printexc.to_string e))

let launch_test no (SystemTest(name, file, expectation)) =
  print_test no name file;
  try
    if Sys.is_directory file then
      print_system_error (sprintf "%s: is a directory" file)
    else
      let f = open_in file in
      test_expectation f expectation;
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
  let xtest = Xml.parse_file "data/test/test.xml" in
  let dtd = Dtd.parse_file "data/test/test.dtd" in
  let valid_xtest = Dtd.prove (Dtd.check dtd) "test" xtest in
  launch_tests (test_of_xml valid_xtest)
with
| Dtd.Check_error(e) -> print_endline (Dtd.check_error e)
| Dtd.Prove_error(e) -> print_endline (Dtd.prove_error e)
| Dtd.Parse_error(e) -> print_endline (Dtd.parse_error e)
| Xml.Error(e) -> print_endline (Xml.error e)
| Xml.File_not_found(s) -> printf "File %s not found.\n" s
