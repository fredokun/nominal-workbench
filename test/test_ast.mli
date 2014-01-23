(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

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
