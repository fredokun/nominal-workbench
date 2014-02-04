(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Vincent Botbol
*)

open Rewriting_ast

let usage = "Usage: nowork [options] <rules files> <terms files>\noptions are:"

let files : string list ref = ref []

let file_argument name =
  if Filename.check_suffix name Config.rule_suffix 
    ||  Filename.check_suffix name Config.term_suffix
  then
    files := name :: !files
  else
    Printf.eprintf "[Warning] Unknown file type : %s. Argument ignored\n" name

let process_file system rfile =
  if not (Sys.file_exists rfile) then 
    (* TODO : CHECK PATH !!!! *)
    begin
      Printf.eprintf "[Warning] %s doesn't exist. Skipping file...\n%!" rfile;
      system;
    end
  else
    begin
      Printf.printf "Evaluating file %s...\n%!" rfile;
      let ic = open_in rfile in
      try
        let structure = Parser.start Lexer.token (Lexing.from_channel ic) in
	let new_system = 
	  List.fold_left
	    Eval.evaluate_structure_item
	    system structure in
        
        close_in ic;
	new_system
      with
      | _ ->
        Printf.eprintf "[Warning] Unhandled error. Skipping %s\n%!" rfile;
        close_in ic;
	system
    end

let main k =
  let open Symbols in
  begin
    (* Parse the command-line *)
    Arg.parse Config.list file_argument usage;

    (* Parse & Eval the files *)
    let system = List.fold_left 
      (fun acc fname -> process_file acc fname)
      empty_system
      (* evaluate in the order given *)
      (List.rev !files)
    in

    (* Read-Eval-Print-Loop *)
    k system
  end
    
