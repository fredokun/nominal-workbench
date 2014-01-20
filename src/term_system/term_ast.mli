(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
*)

type info = Lexing.position

type ident = string

type ('a, 'b) annotated = { value : 'a; info : 'b} 

type 'info expression_raw =
  | Const of ident
  | Abstraction of ident * ident * 'info expression list
  | Call of ident * 'info expression list
  | Var of ident
and 'info expression = ('info expression_raw, 'info) annotated

val mk_dummy : 'a -> ('a, unit) annotated
val mk_node : 'a -> 'b -> ('a, 'b) annotated

val string_of_expression : 'a expression -> string










