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

type term_desc =
  | Const
  | Term of term_ast list
  | Var
and term_ast = 
  {
    name : string;
    info : info;
    desc : term_desc;
  }

val string_of_term : term_ast -> string

val create_term : string -> term_desc -> term_ast
val create_term_info : string -> term_desc -> info -> term_ast
