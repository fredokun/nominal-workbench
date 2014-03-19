(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

(** Utility functions *)

open Parsing_ast

(** Parse an input channel to an AST structure *)
val parse_channel : in_channel -> Parsing_ast.structure
