open Rewriting_ast
open Term_ast

type structure = structure_item list

and structure_item =
| PDecl of rewriting_decl
| PReduce of term * strategy_name
| PTerm of term
| PFile_include of string
| PTermDecl of string * term
(* | Strategy *)
