(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

open Term_ast_typed
open Term_ast_hashconsed
open Rewriting_ast

(* Placeholders bindings *)
module SMap = Map.Make(String)

type placeholders = term_ast_with_binders SMap.t
type hplaceholders = hterm_raw SMap.t

let raise_placeholder_already_defined id =
  let open Rewriting_error in
  raise (RewritingError(PlaceholderAlreadyDefined, id))

let rec combine3 = function
  | [], [], [] -> []
  | hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 ->
    (hd1, hd2, hd3) :: combine3(tl1, tl2, tl3)
  | _, _, _ -> invalid_arg "combine3: different size of lists."

(* Returns None if pattern doesn't match term. Otherwise returns Some(placeholders),
   placeholders being a map containing the placeholders and terms. *)
let matching term pattern =
  let rec step term hashed_term pattern placeholders hplaceholders =

    (* This is impossible that placeholders is equal to None in any of the
       cases below. Proof: We only call "step" in the case of an operator,
       but the fold operation don't recall "step" if placeholders is equal to
       None. The second place we call "step" is in the initialization phase with
       (Some SMap.empty). *)
    let ph = match placeholders with
      | Some (ph) -> ph
      | None -> failwith "Cannot call step with placeholders map set to None." in
    let hph = hplaceholders in (* alias *)

    match term, hashed_term, pattern with
    | DTerm (_, t_id, t_terms), HTerm(_, h_terms), POperator (p_id, p_terms) ->
      if t_id = p_id then
        List.fold_left
          (fun (placeholders, hph) (term, h_term, pattern) ->
            match placeholders with
            | Some ph -> step term h_term.value pattern placeholders hph
            | None -> (None, hph))
          (placeholders, hph)
          (combine3 (t_terms, h_terms, p_terms))
      else (None, hph)

    | DConst (_, t_id), HConst(_), PConstant(p_id) ->
        if t_id = p_id then (placeholders, hph) else (None, hph)

    | DVar (_, v), HVar, PPlaceholder p_id
    | DVar (_, v), HFreeVar _, PPlaceholder p_id ->
      begin
        try
        begin
          match SMap.find p_id ph with
          (* Term variable must have the same name, otherwise they cannot share a same placeholder.
             TODO: Factorize with the _, _, PPlaceholder id matching, it will work when the HVar will be
             correctly hash-consed in every contexts. *)
          | DBinder (_, x) | DVar (_, x) when x = v -> (placeholders, hph)
          | _ -> (None, hph)
        end
        with
        (* This is the first variable with this placeholder we meet. *)
        | Not_found ->
          (Some (SMap.add p_id term ph),
           SMap.add p_id hashed_term hph)
      end

    | DBinder (_, b), HBinder _, PPlaceholder p_id ->
      begin
        try
          (* We probably don't want that binders have the same names? Is there any notion of scope? *)
          ignore @@ SMap.find p_id ph;
          (None, hph)
        with
        | Not_found ->
          (Some (SMap.add p_id term ph),
           SMap.add p_id hashed_term hph)
      end

    | _, _, PPlaceholder id ->
      begin
        try
          let pterm = SMap.find id hph in
          (* We already hit this placeholder, need a term comparison. *)
          if pterm == hashed_term then
            (placeholders, hph)
          else
            (None, hph)
        with
        (* First time we hit this placeholder. *)
        | Not_found ->
          (Some(SMap.add id term ph),
           SMap.add id hashed_term hph)
      end

    | _, _, PAny -> (placeholders, hph)
    | _, _, _ -> (None, hph) in

  let hashed_term = (create_term term).term in
  fst (step term hashed_term pattern (Some SMap.empty) SMap.empty)
