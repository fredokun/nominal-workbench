open Parsetree
open Test

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
    function
      | Term (id, tlist) -> Term(id, List.map (subst_vars system) tlist)
      | Var id as term -> 
      begin
        try
          Term_env.find id !term_env 
        with 
        | Not_found -> term
      end
      | term -> term

and process_term system strategy t =
  let open Term_ast in 
  let open Symbols in
  let open Strategies in
  try
    let strategy = topdown any_rule in
    let nt = Rewriting.rewrite_rec strategy system.rules t in
    Printf.printf "Term : %s rewrote into %s\n%!"
      (string_of_term t)
      (string_of_term nt);
    system
  with
  | _ ->
    Printf.eprintf "Unhandled Term error : %s\n%!" (string_of_term t);
    system

and process_reduce system term strategy =
  let open Rewriting_ast in
  let open Symbols in
  let open Strategies in
    let strategy = match strategy with
    | TopDown -> topdown any_rule
    | BottomUp -> bottomup any_rule
    | Strategy s ->
        let strategy = begin try System_map.find s system.strategies with
        | Not_found -> assert false end in assert false
    in 
    process_term system strategy term 

(* todo : add process_rule + process_directive + process_kind + .. *)

and evaluate_structure_item system =
  let open Rewriting_ast in
  let open Strategies in
  function
  | PInteractiveCmd cmd -> eval_interactive_cmd system cmd
  | PDecl rewriting_decl -> 
    (* ast to modify (shouldn't put a list) *)
    Symbols.enter_decl system rewriting_decl
  | PReduce (term, strategy) ->
      process_reduce system term strategy
  | PTerm term -> process_term system  (topdown any_rule) term
  | PFile_include fname -> process_file system fname
  | PTermDecl (id, term) -> 
      term_env := Term_env.add id term !term_env;
      system

let run_type_check filled_system ast = 
  List.iter (function
    | PDecl d -> 
      Type_checking.check_decl filled_system d
    | _ -> ())
    ast
