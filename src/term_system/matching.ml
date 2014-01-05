(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

open Ast

type atom = string
type dummy_term =
  | D_Const of string
  | D_Atom of atom
  | D_Abs of string * atom * dummy_term list
  | D_App of string * dummy_term list

(* Placeholders bindings *)
module SMap = Map.Make(String)

type placeholders = dummy_term SMap.t

let matching term pattern =
  let rec step term pattern placeholders =
    match term, pattern.value with
    | D_Abs (t_id, _, t_terms), Operator (p_id, p_terms)
    | D_App (t_id, t_terms), Operator (p_id, p_terms) ->
      if t_id = p_id.value then
        List.fold_left
          (fun (matches, placeholders) (term, pattern) ->
             if matches then step term pattern placeholders
             else matches, placeholders)
          (true, placeholders)
        @@ List.combine t_terms p_terms
      else false, placeholders

    | D_Const t_id, Constant p_id -> (t_id = p_id.value, placeholders)

    | _, Placeholder id -> (true, SMap.add id.value term placeholders)

    | _, _ -> (false, placeholders)
  in
  step term pattern SMap.empty
