(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Parsing_ast

val parse_channel : in_channel -> Parsing_ast.structure
