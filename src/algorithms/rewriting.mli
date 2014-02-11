


val rewrite:
  Rewriting_ast.pattern * Rewriting_ast.effect ->
  (Matching.placeholders -> Rewriting_ast.effect -> 'b) ->
  (Term_ast.term -> 'b) -> Term_ast.term -> 'b

  
val rewrite_rec: Strategy.strategy ->
  ('a * (Rewriting_ast.pattern * Rewriting_ast.effect)) Symbols.System_map.t  -> 
  Term_ast.term -> Term_ast.term

                                         
  (* stratgies *)
val bottom_up: (Rewriting_ast.rule -> Term_ast.term -> Term_ast.term)
val top_bottom: (Rewriting_ast.rule -> Term_ast.term -> Term_ast.term)
