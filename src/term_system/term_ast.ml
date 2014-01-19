(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
*)

type info = Lexing.position

type ident = string

type ('a, 'b) annotated = { value : 'a; info : 'b} 

type 'info expression_raw =
  | Const of ident
  | Abstraction of ident * ident * 'info expression list
  | Call of ident * 'info expression list
  | Var of ident
and 'info expression = ('info expression_raw, 'info) annotated

let rec string_of_expression : 'a. 'a expression -> string = 
  function { value = raw_expr; _ } ->
  let rec loop acc = function
    | Const id -> acc^"Constant (" ^ id ^ ")\n"
    | Var id -> acc^"Variable (" ^ id ^ ")\n"

    | Abstraction (name, plh, expr_l) ->
      acc^"Abstraction ("^name^","^plh^",\n"^
	String.concat ",\n" (List.map string_of_expression expr_l)

    | Call (name, expr_l) -> 
      acc^"Call ("^name^",\n"^
	String.concat ",\n" (List.map string_of_expression expr_l)

  in	
  loop "" raw_expr 
