open Rewriting_ast
open Term_ast

type structure = structure_item list

and structure_item =
| PDecl of rewriting_decl
| PTerm of term
| PFile_include of string
(* | Strategy *)
