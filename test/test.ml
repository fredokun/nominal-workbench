(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Printf
open Error_code

type term_test = TermTest of string

type expectation =
  | MustPass of term_test list
  | MustFail of string

type system_test = SystemTest of string * string * expectation

type test = Test of system_test list

type result =
  | Passed
  | Failed of string

(* Parsing utility function *)
let parse_channel channel =
  let lexbuf = Lexing.from_channel channel in
  let res = Parser.start Lexer.token lexbuf in
  close_in channel;
  res

(* Transform XML data to test type. *)
let filter_children xdata name =
  let foreach_child ldata child =
    if name = (Xml.tag child)
    then child :: ldata
    else ldata in
  Xml.fold foreach_child [] xdata

let first_child xdata =
  (List.hd (Xml.children xdata))

let child_data xdata name = 
  Xml.pcdata (first_child (List.hd (filter_children xdata name)))
  
let rec term_tests_of_xml = function
  | [] -> []
  | hd::tl -> TermTest(child_data hd "term") :: (term_tests_of_xml tl)

let expectation_of_xml xtest =
  try MustFail(child_data xtest "error") 
  with Failure(_) -> MustPass(term_tests_of_xml (filter_children xtest "termtest"))

let system_test_of_xml xtest =
  SystemTest(child_data xtest "name", child_data xtest "file", expectation_of_xml xtest)

let test_of_xml xtest =
  Xml.map system_test_of_xml xtest

(* Test framework. *)
let print_system_error e =
  printf "System error: %s\n" e

let print_failure s =
  printf "%s\n" s

let print_success s =
  printf "%s\n" s

let test_expectation channel expectation =
  let open Term_system_error in
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
    let term_system = parse_channel channel in
    match_result_expectation Passed
  with TermSystemError(e, _) ->
    match_result_expectation (Failed(string_of_error_code e))

let launch_test no (SystemTest(name, file, expectation)) =
  printf "Test %d: %s (%s).\n" no name file;
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
