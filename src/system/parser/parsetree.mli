open Rewriting_ast
open Term_ast
open Interactive_ast

type structure = structure_item list

and structure_item =
| PInteractiveCmd of interactive_cmd
| PDecl of rewriting_decl
| PReduce of term * strategy
| PTerm of term
| PFile_include of string
| PTermDecl of string * term
(* | Strategy *)
