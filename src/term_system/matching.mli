(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

type atom = string
type dummy_term =
  | D_Const of string
  | D_Atom of atom
  | D_Abs of string * atom * dummy_term list
  | D_App of string * dummy_term list

type placeholders

val matching : dummy_term -> 'info Ast.term_pattern -> bool * placeholders
