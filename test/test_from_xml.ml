(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Test_ast

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