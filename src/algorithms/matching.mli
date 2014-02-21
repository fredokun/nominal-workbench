(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

type placeholder = PTerm of string | PBinder of string

module SMap : Map.S with type key = placeholder

type placeholders = Term_ast_dag.term_dag SMap.t

(** [matching term pattern] returns a boolean and a map containing the possible
  placeholders if the term matches the pattern given.
    We suppose that the term and the pattern are well-constructed since there is
  a typing phase.
*)
val matching : Term_ast_dag.term_dag -> Rewriting_ast.pattern -> placeholders option
