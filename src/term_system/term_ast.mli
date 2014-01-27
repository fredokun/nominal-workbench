(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
*)

(** Terms Abstract Syntax Tree *)

(** {1 Types} *)

type info = Lexing.position

type ident = string

type term =
  | Const of ident
  | Binder of ident
  | Term of ident * term list
  | Var of ident * (term ref) option (* DAG arrow *)

type term_ast = TermAST of (info * term) list

(** {2 Functions} *)
(*
val mk_dummy : 'a -> ('a, unit) annotated
val mk_node : 'a -> 'b -> ('a, 'b) annotated
*)
val string_of_term : term -> string
