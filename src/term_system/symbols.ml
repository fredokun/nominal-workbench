open Ast

type pos_infos = Lexing.position

let kind_table : (string, pos_infos kind) Hashtbl.t = Hashtbl.create 5
let constant_table :(string, pos_infos const) Hashtbl.t = Hashtbl.create 5
let operator_table :(string, pos_infos operator) Hashtbl.t = Hashtbl.create 5
let rule_table : (string, pos_infos rule) Hashtbl.t = Hashtbl.create 5

exception SymbolAlreadyDefined of pos_infos ident
exception UnknownSymbol of pos_infos ident

let add tbl id =
  if Hashtbl.mem tbl id.value then
    raise (SymbolAlreadyDefined id)
  else
    Hashtbl.add tbl id.value

let add_kind id = add kind_table
let add_constant id = add constant_table
let add_operateor id = add operator_table
let add_rule id = add rule_table

let lookup tbl id =
  try
    Hashtbl.find tbl id.value
  with
  | Not_found -> raise (UnknownSymbol id)

let lookup_kind id = lookup kind_table
let lookup_constant id = lookup constant_table
let lookup_operateor id = lookup operator_table
let lookup_rule id = lookup rule_table
