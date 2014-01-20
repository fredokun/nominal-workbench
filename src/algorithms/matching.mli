(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

module SMap : Map.S with type key = string
type 'info placeholders = 'info Term_ast.expression SMap.t

(** [matching term pattern] returns a boolean and a map containing the possible
  placeholders if the term matches the pattern given.
    We suppose that the term and the pattern are well-constructed since there is
  a typing phase.
*)
val matching : 'info Term_ast.expression -> Rewriting_ast.pattern -> bool * 'info placeholders


