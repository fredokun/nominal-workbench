open Parsetree

(* TODO : mli !! *)

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
      let structure = 
	Parser.start Lexer.token (Lexing.from_channel ic) 
      in
      let new_system = 
	List.fold_left
	  evaluate_structure_item
	  system structure 
      in
      close_in ic;
      new_system
    with
    | _ ->
      Printf.eprintf "[Warning] Unhandled error. Skipping %s\n%!" fname;
      close_in ic;
      system
  end

and process_term system t =
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

and evaluate_structure_item system = function
  | PDecl rewriting_decl -> 
    (* ast to modify (shouldn't put a list) *)
    Symbols.enter_ast system [rewriting_decl]
  | PTerm term -> process_term system term
  | PFile_include fname -> process_file system fname
  
