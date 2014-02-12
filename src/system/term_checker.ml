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
  let (_,args,_) = op in
  let rec give_pos l n = function
    | [] -> l
    | OpTypeArg(_)::q -> give_pos l (n+1) q
    | OpBinderArg(_)::q -> give_pos (n::l) (n+1) q
  in
  give_pos [] 0 args


let rec construct_ast_dag_rec system curr_op pos_in_op (term : Term_ast.term_ast) =
  match term.desc with
  | Const ->
    begin
      ignore (Symbols.lookup_const system term.name);
      DConst term.name
    end
  | Var ->
    let at_binder_pos = List.mem pos_in_op (binders_pos_in_op system curr_op) in
    if at_binder_pos then
      DBinder term.name
    else
      DVar term.name
  | Term(terms) ->
    begin
      let (_,(_,args,_)) = lookup_op system term.name in
      if List.length args != List.length terms then
	raise (TermSystemError(WrongTermArity, Term_ast.string_of_term term));
      DTerm (term.name, List.map (construct_ast_dag_rec system term.name 0) terms)
    end


let construct_term_dag system term =
  construct_ast_dag_rec system "" 0 term

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

(* let rec term_dag_check_types system term_dag = *)
(*   match term_dag with *)
(*     | DConst(ident) -> *)
(*       let (_,Constant(type_binders,const_type)) = lookup_const system ident in *)
(*       let gen_binders = binders_to_TBinds type_binders in *)
(*       (TypedConst(const_type),gen_binders) *)
(*     | DBinder(ident,_) -> (\* TODO *\) *)
(*     | DVar(ident,_) -> (\* TODO *\) *)
(*     | DTerm(ident,sub_terms) -> *)
(*       let (_,Operator(type_binders,args,res)) = lookup_op system ident in *)
(*       let gen_binders = binders_to_TBinds type_binders in *)
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
(* 		  | _ -> TermSystemError (BinderClash, pos_to_string info) *)
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
