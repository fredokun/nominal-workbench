(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Term_ast
open Term_ast_dag
open Symbols
open Rewriting_ast
open Term_system_error

module Var_map = Map.Make(String)

type binder_env = (term_dag ref) Var_map.t

let empty_binder_env = Var_map.empty

(* Return the list of binder positions in the operator op_name *)
let binders_pos_in_op system op_name =
  let (_,op) = Symbols.lookup_op system op_name in
  let Operator(_,args,_) = op in
  let rec give_pos l n = function
    | [] -> l
    | OpTypeArg(_)::q -> give_pos l (n+1) q
    | OpBinderArg(_)::q -> give_pos (n::l) (n+1) q
  in
  give_pos [] 0 args


(* Check if a term is well defined and construct term_ast_dag from a term_ast
Return a couple term_ast_dag * binder_env
It's an auxiliary function *)
let rec construct_ast_dag_rec system curr_op binders term pos_in_op =
  match term with
  | Const(ident) as c ->
    begin
      ignore (Symbols.lookup_const system ident);
      (DConst ident,binders)
    end
  | Var(ident) as v when Var_map.mem ident binders -> (* We already know this var *)
    let at_binder_pos = List.mem pos_in_op (binders_pos_in_op system curr_op) in
    if at_binder_pos then
	(* A new binder, that owerwrite a previous one *)
      let new_binder = DBinder(ident,ref []) in
      let new_binders = Var_map.add ident (ref new_binder) binders in
      (new_binder, new_binders)
    else
	(* A new bounded variable *)
      begin
	let (sons,father) =
	  match !(Var_map.find ident binders) with
	  | DBinder(_,sons) as father -> (sons,father)
	  | _ -> assert false
	in
	let new_var = DVar(ident, Some(ref father)) in
	sons := (ref new_var) :: (!sons);
	(new_var,binders)
      end
  | Var(ident) as v -> (* We don't know this var *)
    let at_binder_pos = List.mem pos_in_op (binders_pos_in_op system curr_op) in
    if at_binder_pos then
	(* A new binder *)
      let new_binder = DBinder(ident, ref []) in
      (new_binder, Var_map.add ident (ref new_binder) binders)
    else
	(* A new free variable *)
      let new_var = DVar(ident, None) in
      (new_var, binders)
  | Term(ident,terms) ->
    let (_,new_binders,terms_well_formed) =
      List.fold_left
	(construct_sub_term system ident)
	(0,binders,[])
	terms
    in (DTerm(ident,List.rev terms_well_formed),binders)
and construct_sub_term = fun system ident ->
  fun (pos,binds,l) sub_term ->
    let (sub_term_wf,new_binds) =
      construct_ast_dag_rec system ident binds sub_term pos in
    (pos+1,new_binds,sub_term_wf::l)

(* Check a term_ast and construct a the new term_ast_dag associated *)
let construct_ast_dag system term_ast =
  let TermAst(info_and_terms) = term_ast in
  let info_and_terms_dag = List.map
    (fun (info,term) ->
      let (term_dag,_) = construct_ast_dag_rec system "" empty_binder_env term 0
      in (info,term_dag))
    info_and_terms
  in
  TermAstDag(info_and_terms_dag)

(* let rec term_check_types system term = *)
