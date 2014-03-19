open Rewriting_ast
open Symbols

(* Check that the AST is well-formed and well-typed.
  Raise RewritingSystemError on error. *)
val check_decl : system -> rewriting_decl -> unit

val check_pattern : system -> pattern ->
  (string * (type_binders * type_application)) list
