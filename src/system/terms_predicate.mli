(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Pierre Talbot *)

val term_inclusion : Symbols.system -> Term_ast.term_ast list -> Term_ast.term_ast list -> bool
val term_equality : Symbols.system -> Term_ast.term_ast list -> Term_ast.term_ast list -> bool
