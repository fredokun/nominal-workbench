(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

open Term_ast_typed
open Rewriting_ast

type placeholder = PTerm of string | PBinder of string

(* Placeholders bindings *)
module SMap = Map.Make(struct
    type t = placeholder
    let compare = Pervasives.compare
end)

type placeholders = term_ast_with_binders SMap.t

let raise_placeholder_already_defined id =
  let open Rewriting_error in
  raise (RewritingError(PlaceholderAlreadyDefined, id))

let matching term pattern =
  let rec step term pattern placeholders =
    match term, pattern with
    | DTerm (_, t_id, t_terms), POperator (p_id, p_terms) ->
      if t_id = p_id then
        List.fold_left
          (fun placeholders (term, pattern) ->
            match placeholders with
            | Some ph -> step term pattern placeholders
            | None -> None)
          placeholders
	  ( List.combine t_terms p_terms )
      else None

    | DConst (_, t_id), PConstant p_id ->
        if t_id = p_id then placeholders else None

    | DVar _, PPlaceholder p_id ->
      let ph = match placeholders with
        | None -> SMap.empty
        | Some ph -> ph in
      Some (SMap.add (PTerm p_id) term ph)

    | _, PPlaceholder id ->
      let ph = match placeholders with
        | None -> SMap.empty
        | Some ph -> ph in
      let pl = match term with
        | DBinder _ -> PBinder id
        | _ -> PTerm id in

      if SMap.mem pl ph then raise_placeholder_already_defined id
      else Some (SMap.add pl term ph)

    | _, PAny -> placeholders
    | _, _ -> None
  in
  step term pattern (Some SMap.empty)
