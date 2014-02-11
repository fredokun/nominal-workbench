


val rewrite:
  Rewriting_ast.pattern * Rewriting_ast.effect ->
  (Matching.placeholders -> Rewriting_ast.effect -> 'b) ->
  (Term_ast.term -> 'b) -> Term_ast.term -> 'b

  
val rewrite_rec: (Rewriting_ast.rule -> Term_ast.term -> Term_ast.term) ->
  Rewriting_ast.rule list -> Term_ast.term -> Term_ast.term

                                         
  (* stratgies *)
val bottom_up: (Rewriting_ast.rule -> Term_ast.term -> Term_ast.term)
val top_down: (Rewriting_ast.rule -> Term_ast.term -> Term_ast.term)
