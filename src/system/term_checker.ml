(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Term_ast
open Term_ast_dag
open Symbols
open Rewriting_ast
open Term_system_error
open Map

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
    | Const(ident) ->
      begin
	ignore (Symbols.lookup_const system ident);
	(DConst ident,binders)
      end
    | Var(ident) when Var_map.mem ident binders -> (* We already know this var *)
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
    | Var(ident) -> (* We don't know this var *)
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

(**)

module TBinders_map = Map.Make(String)

let put_binders_into_map tbinds =
  let map = TBinders_map.empty in
  List.map (fun x -> TBinders_map.add x x map) tbinds

let rec type_to_string t =
  match t with
    | TypeName(s) -> s
    | TypeApplication(g,a) ->
      let parameters = (List.fold_left
			  (fun s t ->  (type_to_string t) ^ s)
			  ""
			  a) in
      g ^ "<" ^ parameters ^ ">"

let type_clash_error t1 t2 =
  let s1 = (type_to_string t1) in
  let s2 = (type_to_string t2) in
  raise (TermSystemError(TypeClash, s1 ^ " and " ^ s2))

(* let compare_typed_term_and_type type_binders typed_term typ = *)
(*   match typed_term with *)
(*     | TypedConst(tapp) -> *)

let rec unify_types type_binders t1 t2 =
  match (t1,t2) with
    | (TypeName(tn1),TypeName(tn2)) ->
      if (tn1 = tn2) then
	(t1,type_binders)
      else
	if (TBinders_map.mem tn1 type_binders) then
	  let t1_map = TBinders_map.find tn1 type_binders in
	  if (TBinders_map.mem tn2 type_binders) then
	    let t2_map = TBinders_map.find tn2 type_binders in
	    if t1_map = tn1 then
	      if t2_map = tn2 then (* t1 and t2 generics *)
		(TypeName(t1_map), type_binders)
	      else (* t1 generic *)
		(TypeName(t2_map), TBinders_map.add tn1 t2_map type_binders) (* unification *)
	    else (* no one generic *)
	      if t1_map = t2_map then
		(TypeName(t1_map), type_binders)
	      else
		type_clash_error t1 t2
	  else
	    if t1_map = tn1 then (* t1 generic *)
	      (t2, TBinders_map.add tn1 tn2 type_binders) (* unification *)
	    else
	      type_clash_error t1 t2
	else
	  type_clash_error t1 t2
    | (TypeApplication(tn1,tb1),TypeApplication(tn2,tb2)) when tn1 = tn2 ->
      let (tb,new_type_binders) = unify_types_list type_binders tb1 tb2 in
      (TypeApplication(tn1,tb),new_type_binders)
    | _ -> type_clash_error t1 t2
and unify_types_list type_binders l1 l2 =
  match (l1,l2) with
    | ([], []) -> ([],type_binders)
    | (t1::q1, t2::q2) -> let (t, new_binders) = unify_types type_binders t1 t2 in
			  let (q, final_binders) = unify_types_list new_binders q1 q2 in
			  (t::q, final_binders)
    | _ -> raise (TermSystemError(WrongTermArity, "todo"))

(* let rec term_check_types system term = *)
(*   let TermAstDag(info,term_dag) = term in *)
(*   match term_dag with *)
(*     | DConst(ident) -> *)
(*       let (_,Constant(type_binders,const_type)) = lookup_const system ident in *)
(*       let gen_binders = binders_to_TBinds type_binders in *)
(*       (\*TODO*\) *)
(*     | DBinder(ident,_) -> (\* TODO *\) *)
(*     | DVar(ident,_) -> (\* TODO *\) *)
(*     | DTerm(ident,sub_terms) -> *)
(*       let (_,Operator(type_binders,args,res)) = lookup_op system ident in *)
(*       let gen_binders = put_binders_into_map type_binders in *)
(* 	List.map2 *)
(* 	(\* begin *\) *)
(* 	(fun term arg -> *)
(* 	  match arg with *)
(* 	    | OpTypeArg(t_app) -> *)
(* 	      begin *)
(* 		let term_typ = term_check_types system term in *)
(* 		match t_app with *)
(* 		  | TypeName(t_name) ->  (\* TODO comparison *\) *)
(* 		  | TypeApplication(typ, app) -> (\* TODO comparison *\) *)
(* 	    | OpBinderArg(name) -> *)
(* 	      begin *)
(* 		match term with *)
(* 		  | DBinder(ident,_) -> TypedBinder ident *)
(* 		  | _ -> Term_system_error (BinderClash, pos_to_string info) *)
(* 	      end *)
(* 	) *)
(* 	(\* end *\) *)
(* 	sub_terms *)
(* 	args *)
(* forall term in subterms :
     if term is TypeApplication position :
       if you know instanciation of the type binder :
         check with it
       else :
         if you get generic type :
           use it
           recheck precedent term
         else :
           continue
     else :
       check type
*)
