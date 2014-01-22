open Rewriting_ast

(* Set the global environment with the declarations of the AST.
  Warn on symbol redeclaration. *)
val set_up_environment : rewriting_ast -> unit

(* Set the global environment with the declarations of the AST.
  Raise a RewritingSystemError on redeclaration. *)
val set_up_environment_strict : rewriting_ast -> unit

(* Check that the AST is well-formed and well-typed.
  Raise RewritingSystemError on error. *)
val check_ast : rewriting_ast -> unit
