(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Def_ast
open Def_ast_util

let is_same_name named_item1 named_item2 = 
  let name1, _ = named_item1 in
  let name2, _ = named_item2 in
  name1 = name2
;;

(* Check that a generic named item was not already defined *)
let check_double_def item visited = 
  let existing = 
    List.filter (fun k -> is_same_name k.value item.value) visited 
  in
  match existing with
  | [] -> ()
  | head::_ -> failwith "Already defined at " (* TODO *)
;;

(* Check a generic list of named items for double definition and with check_fun *)
let check_defs defs check_fun =
  let rec check_defs remaining visited =
    match remaining with
    | [] -> ()
    | head::tail ->
      check_double_def head visited;
      check_fun head;
      check_defs tail (head::visited)
  in
    check_defs defs []
;;

let check_kinds kinds = check_defs kinds ignore;;

let rec check_term_type kinds_env term_type = 
  match term_type.value with
  | Kind(ident) -> begin 
      try
        let kind = Hashtbl.find kinds_env ident.value in
        if kind_kind_type kind == Atom then
          failwith "Atom can only be used in an abstraction" (* TODO *)
        else
          ()
      with Not_found -> failwith "Undefined kind at " (* TODO *)
    end
  | Abs(ident) -> begin
      try
        let kind = Hashtbl.find kinds_env ident.value in
        if kind_kind_type kind <> Atom then
          failwith "Only atoms can be used in an abstraction" (* TODO *)
        else
          ()
      with Not_found -> failwith "Undefined kind at " (* TODO *)
    end
  | Prod(term_types) -> List.iter (check_term_type kinds_env) term_types
;;
    

let check_consts kinds_env consts = 
  let check_kind kind =
    let term_type = const_term_type kind in
    check_term_type kinds_env term_type;
  in
  check_defs consts check_kind
;;

let check_operators kinds_env operators =
  let check_operator operator =
    let left_type, right_type = operator_term_types operator in
    check_term_type kinds_env left_type;
    check_term_type kinds_env right_type
  in
  check_defs operators check_operator
;;

let env_of_kinds kinds =
  let env = Hashtbl.create 10 in
  List.iter (fun k -> Hashtbl.add env (fst k.value) (snd k.value)) kinds;
  env
;;

let check_definitions (kinds, consts, operators, rules) =
  check_kinds kinds;
  let kinds_env = env_of_kinds kinds in
  check_consts kinds_env consts;
  check_operators kinds_env operators;
  (* TODO rules *)
;;
