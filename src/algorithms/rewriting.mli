(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright NoWork team
*)

val rewrite:
  Rewriting_ast.pattern * Rewriting_ast.effect ->
  (Matching.placeholders -> Rewriting_ast.effect -> 'b) ->
  (Term_ast_typed.term_ast_with_binders -> 'b) -> Term_ast_typed.term_ast_with_binders -> 'b


val rewrite_rec: Strategy_ast.strategy ->
  Symbols.system -> Term_ast.term_ast list -> Term_ast.term_ast list
