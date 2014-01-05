(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

type 'info placeholders

(** [matching term pattern] returns a boolean and a map containing the possible
  placeholders if the term matches the pattern given.
    We suppose that the term and the pattern are well-constructed since there is
  a typing phase.
*)
val matching : 'info Term_ast.expression -> 'info Ast.term_pattern
  -> bool * 'info placeholders
