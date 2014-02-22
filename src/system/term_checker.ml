(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Term_ast
open Term_ast_typed
open Symbols
open Rewriting_ast
open Term_system_error
open Rewriting_system_error
open Map
open Utils
open Lexing

let lookup_op sys op =
  try
    Symbols.lookup_op sys op
  with
  | RewritingSystemError (_, desc) -> raise (TermSystemError (UnknownSymbol, desc))

let lookup_rule sys rule =
  try
    Symbols.lookup_rule sys rule
  with
  | RewritingSystemError (_, desc) -> raise (TermSystemError (UnknownSymbol, desc))

let lookup_kind sys kind =
  try
    Symbols.lookup_kind sys kind
  with
  | RewritingSystemError (_, desc) -> raise (TermSystemError (UnknownSymbol, desc))

let lookup_const sys const =
  try
    Symbols.lookup_const sys const
  with
  | RewritingSystemError (_, desc) -> raise (TermSystemError (UnknownSymbol, desc))

let rec string_of_bnd_typ_app indent b =
  let BndTypApp(bnd, t_app) = b in
  "(\n" ^ (TBinders_map.fold (fun k b res -> indent ^ k ^ " : " ^ (string_of_bnd_typ_app (indent ^ "\
  ")  b) ^ res) bnd "") ^ indent ^ "\n" ^ indent ^ "* " ^ Pretty.(string_of pp_type_application t_app) ^ ")\n"


(* Return the list of binder positions in the operator op_name *)
let binders_pos_in_op system op_name =
  let (_,op) = lookup_op system op_name in
  let (_,args,_) = op in
  let rec give_pos l n = function
    | [] -> l
    | OpTypeArg(_)::q -> give_pos l (n+1) q
    | OpBinderArg(_)::q -> give_pos (n::l) (n+1) q
  in
  give_pos [] 0 args


let rec construct_ast_checked_rec
    system curr_op pos_in_op (term : Term_ast.term_ast) =
  match term.desc with
  | Const ->
    begin
      try
	ignore (lookup_op system term.name);
	raise (TermSystemError (WrongTermArity, "todo"))
      with
      | TermSystemError (UnknownSymbol, desc) ->
	ignore (lookup_const system term.name);
	DConst (Some term.info, term.name)
    end
  | Var ->
    let at_binder_pos = List.mem pos_in_op (binders_pos_in_op system curr_op) in
    if at_binder_pos then
      DBinder (Some term.info, term.name)
    else
      DVar (Some term.info, term.name)
  | Term(terms) ->
    begin
      let (_,(_,args,_)) = lookup_op system term.name in
      if List.length args != List.length terms then
	raise (TermSystemError(WrongTermArity, Pretty.(string_of pp_term term)));
      DTerm (Some term.info, term.name, construct_sub_term_checked system term.name terms)
    end

and construct_sub_term_checked system curr_op terms =
  List.rev ( snd (
    List.fold_left
      (fun (n,l) sub_term ->
	((n+1),
	 (construct_ast_checked_rec system curr_op n sub_term)::l))
      (0,[])
      terms))

and construct_ast_checked system term =
  construct_ast_checked_rec system "" 0 term

let binders_to_TBinds tbinds =
  let map = TBinders_map.empty in
  List.fold_left (fun map x -> TBinders_map.add x (BndTypApp (TBinders_map.empty, (TypeName x))) map)
    map tbinds

let type_clash_error ?(pos=Lexing.dummy_pos) t1 t2 =
  let s1 = Pretty.(string_of pp_type_application t1) in
  let s2 = Pretty.(string_of pp_type_application t2) in
  raise (TermSystemError(TypeClash, s1 ^ " and " ^ s2 ^ " at position " ^ (Utils.pos_to_string pos)))

let rec genericity tb t =
  match t with
  | TypeName tn ->
    if (TBinders_map.mem tn tb) then
      let BndTypApp (tb_map, t_map) = TBinders_map.find tn tb in
      if t_map = t then
	if TBinders_map.is_empty tb_map then
	  Gen
	else
	  Inst (BndTypApp (tb_map, t_map))
      else
	Inst (BndTypApp (tb_map, t_map))
    else
      Simple
  | _ -> Simple

let subst_bnd_typ_app bndtypapp =
  let rec subst_bnd_typ_app_rec fresh_name =
    function BndTypApp (bnd, t) ->
      match genericity bnd t with
      | Gen -> (TypeName (String.make 1 (char_of_int fresh_name)), fresh_name + 1)
      | Simple -> (t, fresh_name)
      | Inst (BndTypApp (bnd_subst, t_subst)) ->
	begin
	  match t_subst with
	  | TypeName tn -> subst_bnd_typ_app_rec fresh_name (TBinders_map.find tn bnd_subst)
	  | TypeApplication (tn, tapps) ->
	    let (tapps_subst, fresh) = 
	      List.fold_left
		(fun (tapp, fresh) t ->
		  let (new_t, new_fresh) = subst_bnd_typ_app_rec fresh (BndTypApp (bnd_subst, t)) in
		  (new_t::tapp, new_fresh))
		([],fresh_name)
		tapps
	    in
	    (TypeApplication (tn, List.rev tapps_subst), fresh)
	end
  in
  fst (subst_bnd_typ_app_rec (int_of_char 'A') bndtypapp)

let type_of_typed_term term =
  match term with
  | TypedBinder t -> t
  | TypedVar t -> t
  | TypedConst t -> subst_bnd_typ_app t
  | TypedTerm t -> subst_bnd_typ_app t

let rec merge_tbs_option k tb1_op tb2_op =
  match (tb1_op, tb2_op) with
  | (None, None) -> None
  | (Some t1, None) -> Some t1
  | (None, Some t2) -> Some t2
  | (Some (BndTypApp (tb1, t1) as b1), Some (BndTypApp (tb2, t2) as b2)) ->
    (* print_endline ("key " ^ k ^ "\n" ^ (string_of_bnd_typ_app "" b1) ^ "\n" ^ (string_of_bnd_typ_app "" b2)); *)
    Some (unify_types
	    tb1 tb2
	    (* (TypeName k) *)
	    (* (TypeName k)) *)

	    (* (TBinders_map.add k b1 tb1) *)
	    (* (TBinders_map.add k b2 tb2) *)
	    (* (TBinders_map.add k b1 TBinders_map.empty) *)
	    (* (TBinders_map.add k b2 TBinders_map.empty) *)
	    t1 t2)

and merge_tbs tb1 tb2 =
  TBinders_map.merge merge_tbs_option tb1 tb2

and unify_types tb1 tb2 t1 t2 =
  match t1 with
  | TypeName(tn1) ->
    begin
      match genericity tb1 t1 with
      | Gen -> let inst = BndTypApp (tb2, t2) in
	       BndTypApp (TBinders_map.add tn1 inst tb1, t2)
      | Inst (BndTypApp (tb, t)) -> unify_types tb tb2 t t2
      | Simple ->
	begin
	  match genericity tb2 t2 with
	  | Gen -> BndTypApp (tb1, t1)
	  | Inst (BndTypApp (tb, t)) ->
	    begin
	      match t with
	      | TypeName tn2 when TBinders_map.mem tn2 tb->
		let BndTypApp (_, to_test) = TBinders_map.find tn2 tb in
		if t1 = to_test then
		  BndTypApp (tb1, t1)
		else
		  type_clash_error t1 t2
		  (* raise (TermSystemError(TypeClash, string_of_bnd_typ_app "" (BndTypApp (tb, t)) )) *)
	      | _ -> type_clash_error t1 t2
	    end
	  | Simple -> if t1 = t2 then BndTypApp (tb1, t1) else type_clash_error t1 t2
	end
    end
  | TypeApplication (tn1, tapps1) ->
    begin
      match t2 with
      | TypeApplication (tn2, tapps2) ->
	if tn1 = tn2 then
	  try
	    let new_tbs_apps =
	      List.map2 (unify_types tb1 tb2) tapps1 tapps2 in
	    let (new_tbs, new_apps) =
	      List.fold_right
		(fun (BndTypApp (tb, t)) (tb_acc, apps) -> (merge_tbs tb tb_acc, t::apps))
		new_tbs_apps
		(TBinders_map.empty, [])
	    in
	    BndTypApp (new_tbs, TypeApplication(tn1, new_apps))
	  with
	  | Invalid_argument _ -> type_clash_error t1 t2
	else
	  type_clash_error t1 t2
      | _ -> unify_types tb2 tb1 t2 t1
    end

let rec check_sub_terms system gen_binders =
  fun term arg ->
    match term with
    | DBinder _ ->
      begin
	match arg with
	| OpBinderArg t -> TypedBinder (TypeName t)
	| t -> raise (TermSystemError (TypeClash, "todo" (* todo *) ))
      end
    | DVar _ ->
      begin
	match arg with
	| OpTypeArg t -> TypedVar t
	| _ -> raise (TermSystemError (TypeClash, "todo1" (* todo *) ))
      end
    | DConst (info, ident) ->
      let (_,(type_binders,const_type)) = lookup_const system ident in
      let const_tbinders = binders_to_TBinds type_binders in
      begin
	match arg with
	| OpTypeArg arg_type ->
	  let typ = unify_types gen_binders const_tbinders arg_type const_type in
	  TypedConst typ
	| _ -> raise (TermSystemError (TypeClash, "todo" (* todo *) ))
      end
    | DTerm (info, _, _) as t ->
      begin
	match arg with
	| OpTypeArg arg_type ->
	  begin
	    match check_type_of_term system term with
	    | TypedTerm (BndTypApp (binds,typ)) ->
	      TypedTerm (unify_types gen_binders binds arg_type typ)
	    | _ -> failwith "What the hell with this term ?!"
	  end
	| _ -> raise (TermSystemError (TypeClash, "todo" (* todo *) ))
      end

and check_type_of_term system term_ast =
  match term_ast with
  | DConst (info, ident) ->
    let (_, (type_binders, const_type)) = lookup_const system ident in
    let gen_binders = binders_to_TBinds type_binders in
    (TypedConst (BndTypApp (gen_binders, const_type)))
  | DBinder _ -> failwith("you should not be here")
  | DVar _ -> failwith("you should not be here")
  | DTerm (info, ident, sub_terms) ->
    (* raise (Invalid_argument (string_of_term term_dag)); *)
    let (_,(type_binders,args,res)) = lookup_op system ident in
    let gen_binders = binders_to_TBinds type_binders in
    let new_binders =
      List.fold_left2
	(fun gen_binders term arg ->
	  let sub_term = check_sub_terms system gen_binders term arg in
	  match sub_term with
	  | TypedVar _ -> gen_binders
	  | TypedBinder _ -> gen_binders
	  | TypedConst ((BndTypApp (bnd,_)) as b) ->
	    (* print_endline "sub term :"; *)
	    (* TBinders_map.fold (fun k e r -> print_endline (k ^ " : " ^ (string_of_bnd_typ_app "" e) ^ "\n"); r) bnd (); *)
	    (* print_endline "arg : " ; *)
	    (* TBinders_map.fold (fun k e r -> print_endline (k ^ " : " ^ (string_of_bnd_typ_app "" e) ^ "\n"); r) gen_binders (); *)
	    (* merge_tbs bnd gen_binders *)
	    bnd
	  | TypedTerm (BndTypApp (bnd,_)) -> bnd)
	    (* TBinders_map.iter  *)
	    (* (fun y x ->  print_endline @@  string_of_bnd_typ_app "" x) *)
	    (* bnd *)
	    (* ; bnd (\* merge_tbs bnd gen_binders *\)) *)
	gen_binders
	sub_terms
	args
    in
    (* print_endline (Pretty.(string_of pp_type_application (type_of_typed_term (TypedTerm (BndTypApp (new_binders, res)))))); *)
    TypedTerm (BndTypApp (new_binders, res))


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

(* let binders_to_TBinds tbinds = *)
(*   let map = TBinders_map.empty in *)
(*   List.map (fun x -> TBinders_map.add x x map) tbinds *)

(* let my_binders = binders_to_TBinds ["A"; "B"] *)

(* let empty_binders = TBinders_map.empty *)

(* unify_types empty_binders (TypeName "Int") (TypeName "Int") *)

(* unify_types (TBinders_map.add "A" "A" empty_binders) (TypeApplication ( "List" , [TypeName "A"])) (TypeApplication ("List" , [TypeName "Int"]));; *)

