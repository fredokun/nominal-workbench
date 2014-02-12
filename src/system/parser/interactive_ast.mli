(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

type filename = string
type name = string
type domain = string
type error = Error of domain * name

type expected_term = string

type term_expectation =
  | TMustPass of expected_term
  | TMustFail of error

type term_lib = string
type term_test = TermTest of term_lib list * string * term_expectation

type expectation =
  | MustPass
  | MustFail of error

type rewriting_test = RewritingTest of filename * expectation

type result =
  | Passed
  | Failed of error

type term_result =
  | TPassed of string
  | TFailed of error

type interactive_ast =
| LoadTest of filename * expectation
| Quit
