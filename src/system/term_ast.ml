(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
  (C) Copyright Pierrick Couderc
  (C) Copyright Matthieu Dien
*)

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

let create_term name desc =
  {desc=desc;name=name;info=Lexing.dummy_pos}

let create_term_info name desc info =
  {desc=desc;name=name;info=info}
