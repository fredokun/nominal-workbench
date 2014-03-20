(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Pierre Talbot *)

let sort_hterm_list env terms =
  let hash_consed_term term =
    let checked_term = Term_checker.construct_ast_checked env term in
    Term_ast_hashconsed.create_term checked_term in
  let hashed_terms = List.map hash_consed_term terms in
  Term_ast_hashconsed.sort_hashed_terms hashed_terms

(* Is the set terms inside the set terms'? *)
let term_inclusion_impl terms terms' =
  let rec compare_bucket terms terms' =
    match terms, terms' with
    | [], [] -> true
    | [], _ -> true
    | _, [] -> false
    | hd :: tl, hd' :: tl' when hd = hd' ->
      compare_bucket tl tl'
    | _, _ -> false in
  let rec compare_list terms terms' =
    match terms, terms' with
    | [], [] -> true
    | [], _ -> true
    | _, [] -> false
    | hd :: tl, hd' :: tl' ->
      compare_bucket hd hd' && compare_list tl tl' in
  compare_list terms terms'

let term_inclusion env terms terms' =
  let terms = sort_hterm_list env terms in
  let terms' = sort_hterm_list env terms' in
  term_inclusion_impl terms terms'

let term_equality env terms terms' =
  let terms = sort_hterm_list env terms in
  let terms' = sort_hterm_list env terms' in
  term_inclusion_impl terms terms' &&
  term_inclusion_impl terms' terms
