open Rewriting_ast
open Symbols

(* checks well-formedness of an ast, i.e. legal names and good arity of calls *)
val ast_well_formed : rewriting_ast -> unit

(* type checking *)
val check_ast : rewriting_ast -> unit
