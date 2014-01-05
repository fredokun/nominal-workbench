(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

open Ast
open Term_ast

(* Placeholders bindings *)
module SMap = Map.Make(String)

type 'info placeholders = 'info expression SMap.t

let matching term pattern =
  let rec step term pattern placeholders =
    match term.value, pattern.value with
    | Abstraction (t_id, _, t_terms), Operator (p_id, p_terms)
    | Call (t_id, t_terms), Operator (p_id, p_terms) ->
      if t_id.value = p_id.value then
        List.fold_left
          (fun (matches, placeholders) (term, pattern) ->
             if matches then step term pattern placeholders
             else matches, placeholders)
          (true, placeholders)
        @@ List.combine t_terms p_terms
      else false, placeholders

    | Const t_id, Constant p_id -> (t_id.value = p_id.value, placeholders)

    | _, Placeholder id -> (true, SMap.add id.value term placeholders)

    | _, _ -> (false, placeholders)
  in
  step term pattern SMap.empty
