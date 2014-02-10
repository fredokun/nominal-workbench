(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

open Term_ast
open Rewriting_ast

(* Placeholders bindings *)
module SMap = Map.Make(String)

type placeholders = Term_ast.term SMap.t

let raise_placeholder_already_defined id =
  let open Rewriting_error in
  raise (RewritingError(PlaceholderAlreadyDefined, id))

let matching term pattern =
  let rec step term pattern placeholders =
    match term, pattern with
    | Term (t_id, t_terms), POperator (p_id, p_terms) ->
      if t_id = p_id then
        List.fold_left
          (fun placeholders (term, pattern) ->
            match placeholders with
            | Some ph -> step term pattern placeholders
            | None -> None)
          placeholders
	  ( List.combine t_terms p_terms )
      else None

    | Const t_id, PConstant p_id ->
        if t_id = p_id then placeholders else None

    | _, PPlaceholder id ->
        let ph = match placeholders with None -> SMap.empty
        | Some ph -> ph in
      (* Testing placeholder unicity at typing phase ? *)
      if SMap.mem id ph then raise_placeholder_already_defined id
      else Some (SMap.add id term ph)

    | _, PAny -> placeholders
    | _, _ -> None
  in
  step term pattern (Some SMap.empty)



