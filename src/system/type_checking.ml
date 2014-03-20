(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
   (C) Copyright Yohan Bismuth
*)

open Rewriting_ast
open Rewriting_system_error
open Symbols
open Utils

(*******************)
(* Error utilities *)
(*******************)

let raise_wrong_arity msg =
  raise (RewritingSystemError (
    WrongArity,
    msg))

let raise_unbound_symbol pos id =
  raise (RewritingSystemError (
    UnboundSymbol,
    Printf.sprintf "%s at %s" id (pos_to_string pos)))

let raise_type_clash t1 t2 =
  raise (RewritingSystemError (
    TypeClash,
    Printf.sprintf "%s and %s" (type_to_string t1) (type_to_string t2)))

let raise_wrong_binder_kind id =
  raise (RewritingSystemError (
    WrongBinderKind,
    id))

let raise_type_ill_formed pos id =
  raise (RewritingSystemError (
    TypeIllFormed,
    Printf.sprintf "%s at %s" id (pos_to_string pos)))

let raise_illegal_type_app ta =
  raise (RewritingSystemError (
    IllegalTypeApp,
    type_to_string ta))

let raise_unknown_placeholder id =
  raise (RewritingSystemError (
    UnknownPlaceholder,
    id))

let raise_illegal_kind id pos =
  raise (RewritingSystemError (
    IllegalKind,
    Printf.sprintf "Illegal kind %s %s" id (pos_to_string pos)))





(**********************)
(* Well-form checking *)
(**********************)


let kind_well_formed id pos = function
  | [Atom] -> ()
  | x ->
    if not (List.length x > 0 && List.for_all ((=) Type) x) then
      raise_illegal_kind id pos

let rec type_well_formed sys tb ta =
  match ta with
  (* type binder *)
  | TypeName id when List.mem id tb ->
    if is_kind sys id then
      warn (Printf.sprintf "Type parameter %s with same name as a kind" id)
  (* kind *)
  | TypeName id ->
    ignore (lookup_kind sys id);
  | TypeApplication (id, args) ->
    if List.mem id tb then
      raise_illegal_type_app ta;
    let (pos, kind_types) = lookup_kind sys id in
    if (List.length kind_types - 1 = List.length args) then
      List.iter (type_well_formed sys tb) args
    else
      raise_type_ill_formed pos id
  

let const_well_formed sys info (tb, ta) =
  type_well_formed sys tb ta

let op_arg_well_formed sys tb op_arg =
  match op_arg with
  | OpTypeArg ta -> type_well_formed sys tb ta
  | OpBinderArg id -> ignore (lookup_kind sys id)

let op_well_formed sys info = function
  | tb, op_args, op_res ->
    List.iter (op_arg_well_formed sys tb) op_args;
    type_well_formed sys tb op_res
  

let rec pat_well_formed sys rule_pos bounded_l pat =
  match pat with
  |PAny-> bounded_l
  |PConstant(string)-> ignore (lookup_const sys string); bounded_l
  |PPlaceholder(string)-> string::bounded_l
  |POperator(string,pattern_list)->
    let (pos, (type_binders_list, operator_arg_list, operator_result))
	= lookup_op sys string in
    List.iter (op_arg_well_formed sys type_binders_list) operator_arg_list;
    if List.length pattern_list = List.length operator_arg_list then
      List.fold_left (pat_well_formed sys rule_pos) bounded_l pattern_list
    else
      raise_wrong_arity (string ^ " in pattern, in rule" ^ (pos_to_string rule_pos))

let rec eff_well_formed sys rule_pos bounded_l eff =
    match eff with
    |EConstant(string)-> ignore (lookup_const sys string)
    |EPlaceholder(string)->
      if not (List.mem string bounded_l) then
	raise_unknown_placeholder string
    |EOperator(string, effect_list)->
      let (pos, (type_binders_list, operator_arg_list, operator_result))
	= lookup_op sys string in
      if List.length effect_list = List.length operator_arg_list then 
	List.iter (eff_well_formed sys rule_pos bounded_l) effect_list
      else
	raise_wrong_arity (string ^ " in effect, in rule" ^ (pos_to_string rule_pos))

let rule_well_formed sys info (pat, eff) =
  let bounded_l = pat_well_formed sys info [] pat in
    eff_well_formed sys info bounded_l eff

let decl_well_formed sys = function
  | {info=info; desc=DConstant c; _} -> 
    const_well_formed sys info c
  | {info=info; desc=DOperator op; _} -> 
    op_well_formed sys info op
  | {info=info; desc=DRule r; _} -> 
    rule_well_formed sys info r
  | { name = id; info = info; desc = DKind k } ->
    kind_well_formed id info k
  | _ -> ()





(*********************)
(* Constant checking *)
(*********************)

let check_param_type env param_name ta =
  if List.mem_assoc param_name env then
    if List.assoc param_name env = ta then
      env
    else
      raise_type_clash (TypeName param_name) ta
  else
    (param_name, ta) :: env

let is_atom sys tb = function
  | TypeName id when not (List.mem id tb) ->
    let (_, k) = lookup_kind sys id in
    k = [Atom]
  | _ -> false

let check_type sys (tb, ta) =
  let rec loop = function
    | TypeApplication (_, args) -> List.iter loop args
    | ta -> ()
  in
  match ta with
  | TypeName id when not (List.mem id tb) -> ()
  | _ -> loop ta

let check_const = check_type



(************************)
(* Type checking engine *)
(************************)


let rec subst_type_param param new_ty ta =
  match ta with
  | TypeName id when id = param -> new_ty
  | TypeApplication (id, args) ->
    TypeApplication (id, List.map (subst_type_param param new_ty) args)
  | _ -> ta

let incr_param str =
  assert (String.length str = 1);
  let c = String.get str 0 in
  let c = Char.chr (Char.code c + 1) in
  String.make 1 c

let get_new_param sys tb old =
  let rec loop res =
    if not (List.mem res tb || is_kind sys res)
    then res
    else loop (incr_param res)
  in
  loop old

(* param_env *)
let rec subst_type_params sys tb env ta =
  match ta with
  | TypeName id when List.mem_assoc id env ->
    let tbb, taa = List.assoc id env in
    if tbb = [] then
      (tb, taa)
    else
      List.fold_left (fun (acc_tb, acc_ta) old_param ->
	let new_param = get_new_param sys acc_tb old_param in
	(new_param :: acc_tb, subst_type_param old_param (TypeName new_param) acc_ta)) (tb, taa) tbb
  | TypeApplication (id, args) ->
    let new_tb, new_args = List.fold_right (fun ta (acc_tb, acc_tas) ->
      let tbb, taa = subst_type_params sys acc_tb env ta in
      (tbb, taa :: acc_tas)) args (tb, [])
    in
    (new_tb, TypeApplication (id, new_args))
  | _ -> (tb, ta)
      
let eq_type (tb1, ta1) (tb2, ta2) =
  let rec loop env ta1 ta2 =
    match ta1, ta2 with
    | TypeName id1, TypeName id2 ->
      let b1, b2 = List.mem id1 tb1, List.mem id2 tb2 in
      if b1 != b2 then
	raise Exit
      else if b1 then
	if List.mem_assoc id1 env then
	  if List.assoc id1 env = id2 then
	    env
	  else
	    raise Exit
	else
	  (id1, id2) :: env
      else if id1 = id2 then
	env
      else
	raise Exit
    | TypeApplication (id1, args1), TypeApplication (id2, args2) ->
      if id1 = id2 then
	List.fold_left2 loop env args1 args2
      else
	raise Exit
    | _ -> raise Exit
  in
  try
    ignore (loop [] ta1 ta2); true
  with
  | Exit -> false

(* ta1 is expected, ta2 given *)
let rec unify_types tb1 tb2 env ta1 ta2 =
  match ta1, ta2 with
  | TypeName id1, _ when List.mem id1 tb1 ->
    if List.mem_assoc id1 env then
      let (tbb, taa) = List.assoc id1 env in
      if eq_type (tbb, taa) (tb2, ta2) then
	env
      else
	raise_type_clash taa ta2
    else
      (id1, (tb2, ta2)) :: env
  | TypeName id1, TypeName id2 when List.mem id2 tb2 ->
    raise_type_clash ta1 ta2
  | TypeName id1, TypeName id2 when id1 = id2 ->
    env
  | TypeApplication (id1, tal1), TypeApplication (id2, tal2) when id1 = id2 ->
    List.fold_left2 (unify_types tb1 tb2) env tal1 tal2
  | _ ->
    raise_type_clash ta1 ta2

(*********************)
(* Operator checking *)
(*********************)

let check_op_arg sys tb op_arg =
  match op_arg with
  | OpTypeArg ta -> check_type sys (tb, ta)
  | OpBinderArg id ->
    let (_, k) = lookup_kind sys id in
    if not (k = [Atom]) then raise_wrong_binder_kind id

let check_op sys = function
  | tb, op_args, op_res ->
    List.iter (check_op_arg sys tb) op_args;
    check_type sys (tb, op_res)


(********************)
(* Pattern checking *)
(********************)

let rec type_check_pat sys tb ((param_env, ph_env) as typing_env) pat ta =
  match pat with
  | PAny -> typing_env
  | PConstant id ->
    let (_, (ctb, cta)) = lookup_const sys id in
    (unify_types tb ctb param_env ta cta, ph_env)
  | PPlaceholder id when List.mem_assoc id ph_env ->
    let (tbb, taa) = List.assoc id ph_env in
    if eq_type (tbb, taa) (tb, ta) then
      typing_env
    else
      raise_type_clash taa ta
  | PPlaceholder id ->
    (param_env, (id, (tb, ta)) :: ph_env)
  | POperator (id, patl) ->
    let (_, (tbb, args, result)) = lookup_op sys id in
    let (new_param_env, ph_env) = List.fold_left2 (check_pat_op_arg sys tb) ([], ph_env) patl args in
    let res_tb, res_ta = subst_type_params sys tbb new_param_env result in
    (unify_types tb res_tb param_env ta res_ta, ph_env)


and check_pat_op_arg sys tb typing_env pat op_arg =
  match op_arg with
  | OpTypeArg t ->
    type_check_pat sys tb typing_env pat t
  | OpBinderArg tname ->
    type_check_pat sys tb typing_env pat (TypeName tname)

let type_of_pat sys pat =
  match pat with
  | PAny -> ([], (["A"], TypeName "A"))
  | PConstant id ->
    let (_, (tb, ta)) = lookup_const sys id in
    ([], (tb, ta))
  | PPlaceholder id ->
    let ty = (["A"], TypeName "A") in
    ([(id, ty)], ty)
  | POperator (id, patl) ->
    let (_,  (tb, args, res)) = lookup_op sys id in
    let (param_env, ph_env) = List.fold_left2 (check_pat_op_arg sys tb) ([], []) patl args in
    (ph_env, subst_type_params sys tb param_env res)


let check_pattern sys pat =
  match pat with
  | PAny
  | PConstant _ -> []
  | PPlaceholder id -> [(id, (["A"], TypeName "A"))]
  | POperator (id, patl) ->
    let (_,  (tb, args, _)) = lookup_op sys id in
    let (_, ph_env) = List.fold_left2 (check_pat_op_arg sys tb) ([], []) patl args in
    ph_env


(*******************)
(* Effect checking *)
(*******************)

let rec type_check_eff sys tb ((param_env, ph_env) as typing_env) eff ta =
  match eff with
  | EConstant id ->
    let (_, (ctb, cta)) = lookup_const sys id in
    (unify_types tb ctb param_env ta cta, ph_env)
  | EPlaceholder id when List.mem_assoc id ph_env ->
    let (tbb, taa) = List.assoc id ph_env in
    if eq_type (tbb, taa) (tb, ta) then
      typing_env
    else
      raise_type_clash taa ta
  | EPlaceholder id ->
    (param_env, (id, (tb, ta)) :: ph_env)
  | EOperator (id, effl) ->
    let (_, (tbb, args, result)) = lookup_op sys id in
    let (new_param_env, ph_env) = List.fold_left2 (check_eff_op_arg sys tb) typing_env effl args in
    let res_tb, res_ta = subst_type_params sys tbb new_param_env result in
    (unify_types tb res_tb param_env ta res_ta, ph_env)
    

and check_eff_op_arg sys tb typing_env eff op_arg =
  match op_arg with
  | OpTypeArg t ->
    type_check_eff sys tb typing_env eff t
  | OpBinderArg tname ->
    type_check_eff sys tb typing_env eff (TypeName tname)

let type_of_effect sys ph_env eff =
  match eff with
  | EConstant id -> 
    let (_, (tb, ta)) = lookup_const sys id in
    (tb, ta)
  | EPlaceholder id when List.mem_assoc id ph_env ->
    List.assoc id ph_env
  | EPlaceholder _ ->
    (["A"], TypeName "A")
  | EOperator (id, effl) ->
    let (_,  (tb, args, res)) = lookup_op sys id in
    let (param_env, _) = List.fold_left2 (check_eff_op_arg sys tb) ([], []) effl args in
    subst_type_params sys tb param_env res

let check_effect sys env eff =
  match eff with
  | EConstant _
  | EPlaceholder _ -> ()
  | EOperator (id, effl) ->
    let (_,  (tb, args, _)) = lookup_op sys id in
    ignore (List.fold_left2 (check_eff_op_arg sys tb) ([], []) effl args)


(*****************)
(* Rule checking *)
(*****************)

let check_rule sys (pattern, effect) =
  let env = check_pattern sys pattern in
  check_effect sys env effect

let check_typing sys decl =
  let type_check = function
    | DConstant c -> check_const sys c
    | DOperator op -> check_op sys op
    | DRule r -> check_rule sys r
    | _ -> () 
  in
    type_check decl.desc


(***************)
(* Entry point *)
(***************)

let check_decl sys decl =
  decl_well_formed sys decl;
  check_typing sys decl

