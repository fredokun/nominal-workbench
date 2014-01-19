(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
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
