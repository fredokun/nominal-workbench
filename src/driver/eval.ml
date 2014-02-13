open Parsing_ast
open Printf
open Interactive_cmd

(* TODO : mli !! *)

module Term_env = Map.Make(String)

(* Todo : virer la ref global *)
let term_env = ref Term_env.empty
  
let find_file fname =
  let is_file_in_dir dir =
    Sys.file_exists (dir ^ "/" ^ fname)
  in
  try 
    List.find is_file_in_dir (Config.get_path ())
    ^ "/" ^ fname
  with Not_found -> 
    Printf.eprintf "[Error] Cannot find the file %s\n%!" fname;
    exit 1

let rec process_file system fname =

  let fpath =
    if Filename.is_implicit fname then
      find_file fname
    else if Sys.file_exists fname then
      fname
    else 
    begin
      Printf.eprintf "[Error] Cannot find the file %s\n%!" fname;
      exit 1
    end
  in
  begin
    let ic = open_in fpath in
    try
      let structure = Parser.start Lexer.token (Lexing.from_channel ic) in
      let new_system = 
        List.fold_left evaluate_structure_item system structure in
      close_in ic;
      new_system
    with
    | _ ->
      Printf.eprintf "[Warning] Unhandled error. Skipping %s\n%!" fname;
      close_in ic;
      system
  end

and subst_vars system = 
  let open Term_ast in
    fun term ->
      match term.desc with
      | Term (tlist) -> create_term_info term.name (Term (List.map (subst_vars system) tlist)) term.info
      | Var -> 
      begin
        try
          Term_env.find term.name !term_env 
        with 
        | Not_found -> term
      end
      | _ -> term

and process_term system strategy t =
  let open Term_ast in 
  let open Symbols in
  let open Strategy_ast in
  try
    let nt = Rewriting.rewrite_rec strategy system t in
    Printf.printf "Term : %s rewrote into %s\n%!"
      (string_of_term t)
      (string_of_term nt);
    nt
  with
  | _ ->
    Printf.eprintf "Unhandled Term error : %s\n%!" (string_of_term t);
    t

and process_term_expr system = function
  | PTermLet (ident, term_expr) -> 
    let rewritten_term = process_term_expr system term_expr in
    term_env := Term_env.add ident rewritten_term !term_env;
    rewritten_term
  | PTermRewrite (term_expr, strategy) ->
    let rewritten_subterm = process_term_expr system term_expr in
    (* Printf.printf "%s with %s \n" 
      (Term_ast.string_of_term  rewritten_subterm)
      (Strategy_ast.string_of_strategy strategy); *)
    let rewritten_term = process_term system strategy rewritten_subterm in
    rewritten_term 
  | PTerm (term) -> term

(* todo : add process_rule + process_directive + process_kind + .. *)

and evaluate_structure_item system =
  let open Rewriting_ast in
  let open Strategy_ast in
  function
  | PInteractiveCmd cmd -> eval_interactive_cmd process_term_expr eval_and_check system cmd
  | PDecl rewriting_decl -> 
    (* ast to modify (shouldn't put a list) *)
    Symbols.enter_decl system rewriting_decl
  | PTermExpr term -> ignore (process_term_expr system term); system
  | PFile_include fname -> process_file system fname

and run_type_check filled_system ast = 
  List.iter (function
    | PDecl d -> 
      Type_checking.check_decl filled_system d
    | _ -> ())
    ast

and eval_and_check system ast = 
  let new_system = List.fold_left evaluate_structure_item system ast in
  run_type_check new_system ast
