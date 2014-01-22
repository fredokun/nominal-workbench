(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Mathieu Chailloux
*)

open Rewriting_ast
open Rewriting_system_error

type 'a sym_tbl = (string, info * 'a) Hashtbl.t

let kind_table : kind sym_tbl = Hashtbl.create 5
let constant_table : constant sym_tbl = Hashtbl.create 5
let operator_table : operator sym_tbl = Hashtbl.create 5
let rule_table : rule sym_tbl = Hashtbl.create 5


let raise_unknown_symbol kind id =
  raise (RewritingSystemError (
    UnknownSymbol,
    kind ^ " " ^ id))

let warn_on_redeclaration id =
  Format.printf "Warning : Symbol %s is already defined.\n" id

let raise_on_redeclaration id =
  raise (RewritingSystemError (RedeclaredSymbol, id))

let add_symbol_impl (name, info, desc) redeclaration_policy  =
  let aux tbl id value =
    if Hashtbl.mem tbl id then
      redeclaration_policy id
    else
      Hashtbl.add tbl id value
  in
  match desc with
  | DKind k -> aux kind_table name (info, k)
  | DConstant c -> aux constant_table name (info, c)
  | DOperator op -> aux operator_table name (info, op)
  | DRule r -> aux rule_table name (info, r)

let add_symbol symbol = add_symbol_impl symbol warn_on_redeclaration
let add_symbol_strict symbol = add_symbol_impl symbol raise_on_redeclaration

let lookup tbl sym_kind id =
  try
    Hashtbl.find tbl id
  with
  | Not_found -> raise_unknown_symbol sym_kind id

let lookup_kind = lookup kind_table "kind"
let lookup_const = lookup constant_table "const"
let lookup_op = lookup operator_table "operator"
let lookup_rule = lookup rule_table "rule"

let exists tbl id = Hashtbl.mem tbl id
let is_kind = exists kind_table
let is_const = exists constant_table
let is_op = exists operator_table
let is_rule = exists rule_table

