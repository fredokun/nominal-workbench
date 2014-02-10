(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

module SMap : Map.S with type key = string

type placeholders = Term_ast.term SMap.t

(** [matching term pattern] returns a boolean and a map containing the possible
  placeholders if the term matches the pattern given.
    We suppose that the term and the pattern are well-constructed since there is
  a typing phase.
*)
val matching : Term_ast.term -> Rewriting_ast.pattern -> placeholders option


