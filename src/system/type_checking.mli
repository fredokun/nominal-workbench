(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright NoWork team
*)

open Rewriting_ast
open Symbols

(* Check that the AST is well-formed and well-typed.
  Raise RewritingSystemError on error. *)
val check_decl : system -> rewriting_decl -> unit

(* Only used by the :match --with command to check that the pattern given is
  well-formed and well-typed *)
val check_pattern : system -> pattern ->
  (string * (type_binders * type_application)) list
