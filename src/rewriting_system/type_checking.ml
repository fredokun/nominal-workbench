(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux)
   (C) Copyright Yohan Bismuth
*)

open Rewriting_ast
open Rewriting_system_error
open Symbols

exception TMP of string

let set_up_environment_impl add_decl = function
  | RewritingAST decls -> List.iter add_decl decls

let set_up_environment ast =
  set_up_environment_impl add_symbol ast

let set_up_environment_strict ast =
  set_up_environment_impl add_symbol_strict ast

let rec type_to_string = function
  | TypeName id -> id
  | TypeApplication (id, args) ->
    id ^ "<" ^ List.fold_left (fun acc t -> acc ^ "," ^ type_to_string t) "" args  ^ ">" 

let raise_wrong_arity op_id =
  raise (RewritingSystemError (
    WrongArity,
    op_id))

let raise_unknown_placeholder id =
  raise (RewritingSystemError (
    UnknownSymbol,
    "placeholder " ^ id))

let raise_type_clash t1 t2 =
  raise (RewritingSystemError (
    TypeClash,
    type_to_string t1 ^ " and " ^ type_to_string t2))


(* Well-form checking *)

let rec check_operator_pat pl ol bounded_l = 
  if (not(List.length pl = List.length ol)) then
    raise (TMP "Wrong arity")
  else
    List.fold_left (fun a b -> pat_well_formed a b) bounded_l pl

and pat_well_formed bounded_l pat =
  match pat with
  |PAny-> bounded_l
  |PConstant(string)-> ignore (lookup_const string); bounded_l
  |PPlaceholder(string)-> string::bounded_l
  |POperator(string,pattern_list)->
    let Operator (type_binders_list, operator_arg_list, operator_result)
	= snd (lookup_op string) in
    check_operator_pat pattern_list operator_arg_list bounded_l

and eff_well_formed bounded_l eff =
    match eff with
    |EConstant(string)->
      ignore (lookup_const string);
      bounded_l
    |EPlaceholder(string)->
      if (List.mem string bounded_l) then bounded_l else raise (TMP "Unknown placeholder")
    |EOperator(string, effect_list)->
      let Operator(type_binders_list, operator_arg_list, operator_result)
	= snd (lookup_op string) in
      if List.length effect_list = List.length operator_arg_list then 
	List.fold_left (fun acc eff -> eff_well_formed acc eff) bounded_l effect_list
      else
	raise_wrong_arity string

let rule_well_formed = function
  |Rule(pat, eff)->
    let bounded_l = pat_well_formed [] pat in
    eff_well_formed bounded_l eff

let rec decl_list_well_formed decll= match decll with
  |(_,_,DRule(rule))::q ->
    ignore (rule_well_formed rule);
    decl_list_well_formed q
  |_-> ()

let ast_well_formed = function
  | RewritingAST decls -> decl_list_well_formed decls




(* Type checking *)

let check_types (tb1, ta1) (tb2, ta2) =
  let rec loop param_env t1 t2 =
    match t1, t2 with
    | TypeName id1, TypeName id2 ->
    (* ta1 and ta2 are type parameters *)
      if List.mem id1 tb1 && List.mem id2 tb2 then
	if List.mem_assoc id1 param_env then
	  if List.assoc id1 param_env = id2
	  then param_env
	  else raise_type_clash t1 t2
	else
	  (id1, id2) :: param_env
    (* ta1 and ta2 are kinds *)
      else if id1=id2 then param_env
      else raise_type_clash t1 t2
    | TypeApplication (id1, args1), TypeApplication (id2, args2) ->
      if id1 = id2 then
	List.fold_left2 loop param_env args1 args2
      else
	raise_type_clash t1 t2
    | _ -> raise_type_clash t1 t2
  in
  ignore (loop [] ta1 ta2)

(* Constant checking *)

let type_check_const id (tb, ta) =
  let (_, Constant (ctb, cta)) = lookup_const id in
  check_types (ctb, cta) (tb, ta)

(* Pattern checking *)

let rec type_check_pat env pat (tb, ta) =
  match pat with
  | PAny -> env
  | PConstant id ->
    type_check_const id (tb, ta);
    env
  | PPlaceholder id when List.mem_assoc id env ->
    check_types (List.assoc id env) (tb, ta);
    env
  | PPlaceholder id ->
    (id, (tb, ta)) :: env
  | POperator (id, patl) ->
    let (_, Operator (tbb, args, result)) = lookup_op id in
    check_types (tbb, result) (tb, ta);
    List.fold_left2 (check_pat_op_arg tb) env patl args

and check_pat_op_arg tb env pat op_arg =
  match op_arg with
  | OpTypeArg t -> type_check_pat env pat (tb, t)
  | OpBinderArg id -> env (* TODO *)
    
  
    
let check_pat pat =
  match pat with
  | POperator (id, patl) ->
    let (_, Operator (tb, args, _)) = lookup_op id in
    List.fold_left2 (check_pat_op_arg tb) [] patl args
  | _ -> []


(* Effect checking *)

let rec type_check_eff env eff (tb, ta) =
  match eff with
  | EConstant id ->
    type_check_const id (tb, ta)
  | EPlaceholder id ->
    check_types (List.assoc id env) (tb, ta)
  | EOperator (id, effl) ->
    let (_, Operator (tbb, args, result)) = lookup_op id in
    check_types (tbb, result) (tb, ta);
    List.iter2 (check_eff_op_arg tb env) effl args

and check_eff_op_arg tb env eff op_arg =
  match op_arg with
  | OpTypeArg t -> type_check_eff env eff (tb, t)
  | OpBinderArg id -> () (* TODO *)
    

let check_eff env eff =
  match eff with
  | EOperator (id, effl) ->
    let (_, Operator (tb, args, _)) = lookup_op id in
    List.iter2 (check_eff_op_arg tb env) effl args
  | _ -> ()

(* Rule checking *)

let check_rule = function
  | Rule (pat, eff) ->
    let env = check_pat pat in
    check_eff env eff
