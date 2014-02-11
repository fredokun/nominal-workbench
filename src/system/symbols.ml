(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast
open Rewriting_system_error
open Utils

(* System definition *)

module System_map = Map.Make(String)

type system = {
  kinds :  (info * kind) System_map.t;
  constants : (info * constant) System_map.t;
  operators : (info * operator) System_map.t;
  rules :(info * rule) System_map.t;
  strategies : strategy System_map.t;
}

let builtin_strategies m = m
              |> System_map.add "topdown" TopDown
              |> System_map.add "top-down" TopDown
              |> System_map.add "bottomup" BottomUp
              |> System_map.add "bottom-up" BottomUp

    
let empty_system = {
  kinds = System_map.empty;
  constants = System_map.empty;
  operators = System_map.empty;
  rules = System_map.empty;
  strategies = builtin_strategies System_map.empty;
}


  
(* Error utilities *)

let raise_unknown_symbol pos sym_kind id =
  raise (RewritingSystemError (
    UnboundSymbol,
    Printf.sprintf "%s %s at %s." sym_kind id (pos_to_string pos)))

let warn_on_redeclaration symbol_category id pos =
  Format.printf "Warning : %s %s at %s is already defined.\n"
    symbol_category id (pos_to_string pos)

let raise_on_redeclaration symbol_category id pos =
  raise (RewritingSystemError (RedeclaredSymbol, 
    Printf.sprintf "[%s] %s %s" (pos_to_string pos) symbol_category id))

(* System establishment *)

let add_kind redeclaration_policy sys id (info, value) =
  if System_map.mem id sys.kinds then
    begin
      redeclaration_policy "kind" id info; sys
    end
  else
    {sys with kinds = System_map.add id (info, value) sys.kinds}

let add_constant redeclaration_policy sys id (info, value) =
  if System_map.mem id sys.constants then
    begin
      redeclaration_policy "constant" id info; sys
    end
  else
    {sys with constants = System_map.add id (info, value) sys.constants}

let add_operator redeclaration_policy sys id (info, value) =
  if System_map.mem id sys.operators then
    begin
      redeclaration_policy "operator" id info; sys
    end
  else
    {sys with operators = System_map.add id (info, value) sys.operators}

let add_rule redeclaration_policy sys id (info, value) =
  if System_map.mem id sys.rules then
    begin
      redeclaration_policy "rule" id info; sys
    end
  else
    {sys with rules = System_map.add id (info, value) sys.rules}

let add_symbol_impl redeclaration_policy system 
    {name=name; info = info; desc=desc} =
  match desc with
  | DKind k ->
    add_kind redeclaration_policy system name (info, k)
  | DConstant c ->
    add_constant redeclaration_policy system name (info, c)
  | DOperator op ->
    add_operator redeclaration_policy system name (info, op)
  | DRule r ->
    add_rule redeclaration_policy system name (info, r)

(*let add_symbol_impl redeclaration_policy system (name, info, desc) =
  let aux (map : (info * 'a) System_map.t) symbol_category value : (info * 'a) System_map.t =
    if System_map.mem name map then
      redeclaration_policy symbol_category name info
    else
      System_map.add name (info, value) map
  in
  match desc with
  | DKind k ->
    {system with kinds = aux system.kinds "Kind" k}
  | DConstant c ->
    {system with constants = aux system.constants "Constant" c}
  | DOperator op ->
    {system with operators = aux system.operators "Operator" op}
  | DRule r ->
    {system with rules = aux system.rules "Rule" r}*)

let add_symbol = add_symbol_impl warn_on_redeclaration
let add_symbol_strict = add_symbol_impl raise_on_redeclaration

let enter_decl_impl add_decl sys = add_decl sys

let enter_decl sys decl =
  enter_decl_impl add_symbol sys decl

let enter_decl_strict sys decl =
  enter_decl_impl add_symbol_strict sys decl


(* System access *)

let lookup map sym_kind ?(pos=Lexing.dummy_pos) id =
  try
    System_map.find id map
  with
  | Not_found -> raise_unknown_symbol pos sym_kind id

let lookup_kind sys = lookup sys.kinds "kind"
let lookup_const sys = lookup sys.constants "const"
let lookup_op sys = lookup sys.operators "operator"
let lookup_rule sys = lookup sys.rules "rule"

let exists map id = System_map.mem id map
let is_kind sys = exists sys.kinds
let is_const sys = exists sys.constants
let is_op sys = exists sys.operators
let is_rule sys = exists sys.rules
