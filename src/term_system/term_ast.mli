(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
  (C) Copyright Pierrick Couderc
  (C) Copyright Matthieu Dien
*)

(** Terms Abstract Syntax Tree *)

(** {1 Types} *)

type info = Lexing.position

type ident = string

type term =
  | Const of ident
  | Term of ident * term list
  | Var of ident

type term_ast = TermAst of (info * term) list

(** {2 Functions} *)
(*
val mk_dummy : 'a -> ('a, unit) annotated
val mk_node : 'a -> 'b -> ('a, 'b) annotated
*)
val string_of_term : term -> string
