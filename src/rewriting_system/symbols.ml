(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast
open Rewriting_system_error
open Utils


type 'a sym_tbl = (string, info * 'a) Hashtbl.t

let kind_table : kind sym_tbl = Hashtbl.create 5
let constant_table : constant sym_tbl = Hashtbl.create 5
let operator_table : operator sym_tbl = Hashtbl.create 5
let rule_table : rule sym_tbl = Hashtbl.create 5


let raise_unknown_symbol pos kind id =
  raise (RewritingSystemError (
    RewritingUnboundSymbol,
    kind ^ " " ^ id ^ (pos_to_string pos)))

let warn sym_kind id pos =
  Format.printf "Warning : %s %s %s is already defined.\n"
    sym_kind id (pos_to_string pos)


let enter_decl (name, info, desc)  =
  let aux tbl sym_kind value =
    if Hashtbl.mem tbl name then
      warn sym_kind name info
    else
      Hashtbl.add tbl name value
  in
  match desc with
  | DKind k -> aux kind_table "Kind" (info, k)
  | DConstant c -> aux constant_table "Constant" (info, c)
  | DOperator op -> aux operator_table "Operator" (info, op)
  | DRule r -> aux rule_table "Rule" (info, r)

let enter_ast = function
  | RewritingAST decls ->
    List.iter enter_decl decls

let clear_symbols () =
  Hashtbl.reset kind_table;
  Hashtbl.reset constant_table;
  Hashtbl.reset operator_table;
  Hashtbl.reset rule_table


let lookup tbl sym_kind ?(pos=Lexing.dummy_pos) id =
  try
    Hashtbl.find tbl id
  with
  | Not_found -> raise_unknown_symbol pos sym_kind id

let lookup_kind = lookup kind_table "kind"
let lookup_const = lookup constant_table "const"
let lookup_op = lookup operator_table "operator"
let lookup_rule = lookup rule_table "rule"

let exists tbl id = Hashtbl.mem tbl id
let is_kind = exists kind_table
let is_const = exists constant_table
let is_op = exists operator_table
let is_rule = exists rule_table

(* tmp *)
let list_of_rules () =
  Hashtbl.fold
    (fun _ (_, v) acc -> v :: acc)
    rule_table
    []
