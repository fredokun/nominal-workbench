open Rewriting_ast

let rec type_to_string = function
  | TypeName id -> id
  | TypeApplication (id, args) ->
    id ^ "<" ^ List.fold_left (fun acc t -> acc ^ "," ^ type_to_string t) "" args  ^ ">" 

let pos_to_string pos =
  let open Lexing in
  Printf.sprintf "l.%d, c.%d, %s"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol) pos.pos_fname

let warn msg = 
Printf.printf "Warning : %s.\n" msg
