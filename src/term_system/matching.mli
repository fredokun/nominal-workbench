(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

type 'info placeholders

val matching : 'info Term_ast.expression -> 'info Ast.term_pattern
  -> bool * 'info placeholders
