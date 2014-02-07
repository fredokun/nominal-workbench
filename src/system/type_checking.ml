(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
   (C) Copyright Yohan Bismuth
*)

open Rewriting_ast
open Rewriting_system_error
open Symbols
open Utils

(* Error utilities *)

let raise_wrong_arity msg =
  raise (RewritingSystemError (
    WrongArity,
    msg))

let raise_unbound_symbol pos id =
  raise (RewritingSystemError (
    UnboundSymbol,
    Printf.sprintf "%s at %s." id (pos_to_string pos)))

let raise_type_clash t1 t2 =
  raise (RewritingSystemError (
    TypeClash,
    Printf.sprintf "%s and %s." (type_to_string t1) (type_to_string t2)))

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

let raise_kind_clash ta k =
  raise (RewritingSystemError (
    KindClash,
    Printf.sprintf "Type %s is not of kind %s" (type_to_string ta) (kind_to_string k)))

let my_raise msg =
  raise (RewritingSystemError (
    ToDoExn,
    msg))


(*
  Well-form checking
*)

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
    (* TO CHECK : empty kind_types : is it possible ? *)
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
  | _ -> ()

(* Kind checking *)

let rec last = function
  | [x] -> x
  | x :: xs -> last xs
  | [] -> assert false

let rec kind_of_type sys tb env ta =
  match ta with
  | TypeName id when List.mem id tb ->
    if List.mem_assoc id env then
      kind_of_type sys tb env (List.assoc id env)
    else
      my_raise "Type_checking.kind_of_type : need kind Any"
  | TypeName id ->
    snd (lookup_kind sys id)
  | TypeApplication (id, _) ->
    let (_, kind_types) = lookup_kind sys id in
    [last kind_types]

let kind_check_type sys tb env ta k =
  let rec loop env ta k =
    match ta, k with
    | TypeName id, _ when List.mem id tb ->
      (* is second test useful ? *)
      if List.mem_assoc id env then
	if List.assoc id env = ta then
	  env
	else
	  loop env (List.assoc id env) k
      else
	(id, ta) :: env
    | TypeName id, _ ->
      let (_, kind) = lookup_kind sys id in
      if k = kind
      then env
      else raise_kind_clash ta k
    | TypeApplication (id, []), [kind_res] ->
      env
    | TypeApplication (id, arg1::args), (k1 :: ks) ->
      let new_env = loop env arg1 [k1] in
      loop new_env (TypeApplication (id, args)) ks
    | _ -> assert false
  in
  loop env ta k
	
(* Type checking *)
      
let check_param_type env param_name ta =
  if List.mem_assoc param_name env then
    if List.assoc param_name env = ta then
      env
    else
      raise_type_clash (TypeName param_name) ta
  else
    (param_name, ta) :: env

let check_types (tb1, ta1) (tb2, ta2) =
  let rec loop (env1, env2) ta1 ta2 =
    match ta1, ta2 with
    | TypeName id, _  when List.mem id tb1 ->
      (check_param_type env1 id ta2, env2)
    | _, TypeName id when List.mem id tb2 ->
      (env1, check_param_type env2 id ta1)
    | TypeName id1, TypeName id2 ->
      if id1 = id2 then
	(env1, env2)
      else
	raise_type_clash ta1 ta2

    | TypeApplication (id1, args1), TypeApplication (id2, args2) ->
      if id1 = id2 then
	List.fold_left2 loop (env1, env2) args1 args2
      else
	raise_type_clash ta1 ta2
    | _ -> raise_type_clash ta1 ta2
  in
  ignore (loop ([], []) ta1 ta2)

   

let check_type sys tb env ta =
  match ta with
  | TypeName id when List.mem id tb ->
    if List.mem_assoc id env then
      env
    else
      (id, ta) :: env
  | TypeName id -> env
  | TypeApplication (id, args) ->
    let (_, k) = lookup_kind sys id in
    kind_check_type sys tb env ta k


(* Constant checking *)

let check_const sys = function
  | tb, ta -> ignore (check_type sys tb [] ta)

let type_check_const sys id (tb, ta) =
  let (_, (ctb, cta)) = lookup_const sys id in
  check_types (ctb, cta) (tb, ta)

(* Operator checking *)

let check_op_arg sys tb env op_arg =
  match op_arg with
  | OpTypeArg ta -> check_type sys tb env ta
  | OpBinderArg id ->
    let (_, k) = lookup_kind sys id in
    if k = [Atom]
    then env
    else raise_wrong_binder_kind id

let check_op sys = function
  | tb, op_args, op_res ->
    let env = List.fold_left (check_op_arg sys tb) [] op_args in
    ignore (check_type sys tb env op_res)

(* Pattern checking *)

let rec type_check_pat sys env pat (tb, ta) =
  match pat with
  | PAny -> env
  | PConstant id ->
    type_check_const sys id (tb, ta);
    env
  | PPlaceholder id when List.mem_assoc id env ->
    check_types (List.assoc id env) (tb, ta);
    env
  | PPlaceholder id ->
    (id, (tb, ta)) :: env
  | POperator (id, patl) ->
    let (_, (tbb, args, result)) = lookup_op sys id in
    check_types (tbb, result) (tb, ta);
    List.fold_left2 (check_pat_op_arg sys tb) env patl args

and check_pat_op_arg sys tb env pat op_arg =
  match op_arg with
  | OpTypeArg t -> type_check_pat sys env pat (tb, t)
  | OpBinderArg id -> type_check_pat sys env pat (tb, TypeName id)
    
let check_pattern sys pat =
  match pat with
  | POperator (id, patl) ->
    let (_,  (tb, args, _)) = lookup_op sys id in
    List.fold_left2 (check_pat_op_arg sys tb) [] patl args
  | _ -> []


let print_env env =
  Format.printf "[";
  List.iter (fun (id,(_,ta)) -> Format.printf "%s : %s, " id (type_to_string ta)) env;
  Format.printf "]"

(* Effect checking *)

let rec type_check_eff sys env eff (tb, ta) =
  match eff with
  | EConstant id ->
    type_check_const sys id (tb, ta)
  | EPlaceholder id ->
    check_types (List.assoc id env) (tb, ta)
  | EOperator (id, effl) ->
    let (_, (tbb, args, result)) = lookup_op sys id in
    check_types (tbb, result) (tb, ta);
    List.iter2 (check_eff_op_arg sys tb env) effl args

and check_eff_op_arg sys tb env eff op_arg =
  match op_arg with
  | OpTypeArg t -> type_check_eff sys env eff (tb, t)
  | OpBinderArg id -> type_check_eff sys env eff (tb, TypeName id)


let check_effect sys env eff =
  match eff with
  | EOperator (id, effl) ->
    let (_, (tb, args, _)) = lookup_op sys id in
    List.iter2 (check_eff_op_arg sys tb env) effl args
  | _ -> ()

(* Rule checking *)
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

(* Checking interface *)
let check_decl sys decl =
  decl_well_formed sys decl;
  check_typing sys decl
