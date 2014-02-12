(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Rewriting_ast
open Strategy_ast
open Term_ast

type filename = string
type name = string
type domain = string
type error = Error of domain * name

type term_expr = 
| PTermLet of string * term_expr
| PTermRewrite of term_expr * strategy
| PTerm of term_ast

type term_predicate = 
  | InPredicate of term_expr * term_expr
  | EqualPredicate of term_expr * term_expr

type term_test =
  | TMustPass of term_predicate
  | TMustFail of term_expr * error

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
| TermTest of term_test
| Quit

type structure = structure_item list

and structure_item =
| PInteractiveCmd of interactive_ast
| PDecl of rewriting_decl
| PTermExpr of term_expr
| PFile_include of string
(* | Strategy *)
