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


let kt_to_string = function
  | Type -> "Type"
  | Atom -> "Atom"

let rec kind_to_string = function
   [] -> assert false
  | k::rem ->
    List.fold_left (fun acc kt ->
      acc ^ " -> " ^ (kt_to_string kt))
      (kt_to_string k) rem
  

let print_kind k =
  Format.printf "[";
  List.iter (fun kt -> Format.printf "%s, " (kt_to_string kt)) k;
  Format.printf "]\n"
