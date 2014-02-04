open Parsetree

(* to delete
let is_directive lb = 
  let str = lb.Lexing.lex_buffer in
  let len = String.length str in
  let cpt = ref 0 in
  while !cpt < len && str.[!cpt] = ' ' do incr cpt done;
  (!cpt < len && str.[!cpt] = ':', !cpt)
(*
let preparse_phrase lb =
  let (b, pos) = is_directive lb in
  if b then
*)
*)

let process_term system t =
  let open Term_ast in 
  let open Symbols in
  try
    let rules = List.map (fun (_, (_, v)) -> v)
      (System_map.bindings system.rules)
    in
    let nt = Rewriting.rewrite_rec rules t in
    Printf.printf "Term : %s rewrote into %s\n%!"
      (string_of_term t)
      (string_of_term nt);
    system
  with
  | _ ->
    Printf.eprintf "Unhandled Term error : %s\n%!" (string_of_term t);
    system

(* todo : add process_rule + process_directive + process_kind + .. *)

let evaluate_structure_item system = function
  | PDecl rewriting_decl -> 
    (* to modify (shouldn't put a list) *)
    Symbols.enter_ast system [rewriting_decl]
  | PTerm term -> process_term system term
  | PFile_include fname -> system (* todo *)
  
