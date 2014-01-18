open Ast

type 'a sym_tbl = (string, info * 'a) Hashtbl.t

let kind_table : kind sym_tbl = Hashtbl.create 5
let constant_table : constant sym_tbl = Hashtbl.create 5
let operator_table : operator sym_tbl = Hashtbl.create 5
let rule_table : rule sym_tbl = Hashtbl.create 5

exception SymbolAlreadyDefined of string
exception UnknownSymbol of string

let add (name, info, desc)  =
  let aux tbl id value =
    if Hashtbl.mem tbl id then
      raise (SymbolAlreadyDefined id)
    else
      Hashtbl.add tbl id value
  in
  match desc with
  | DKind k -> aux kind_table name (info, k)
  | DConstant c -> aux constant_table name (info, c)
  | DOperator op -> aux operator_table name (info, op)
  | DRule r -> aux rule_table name (info, r)


let lookup tbl id =
  try
    Hashtbl.find tbl id
  with
  | Not_found -> raise (UnknownSymbol id)

let lookup_kind = lookup kind_table
let lookup_const = lookup constant_table
let lookup_op = lookup operator_table
let lookup_rule = lookup rule_table
