(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Roven Gabriel
  (C) Copyright Vincent Botbol
*)

let string_of_expression { value = raw_expr; _ } =
  let rec loop acc = function
    | Const id -> acc^"Constant (" ^ id ^ ")\n"
    | Var id -> acc^"Variable (" ^ id ^ ")\n"

    | Abstraction (name, plh, expr_l) ->
      acc^"Abstraction ("^name^","^plh^",\n"^
	String.concat ",\n" (List.map (loop "") expr_l)

    | Call (name, expr_l) -> 
      acc^"Call ("^name^",\n"^
	String.concat ",\n" (List.map (loop "") expr_l)

  in	
  loop "" raw_expr 
