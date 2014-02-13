(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Vincent Botbol
*)

open Printf

open Rewriting_ast

let usage = "Usage: nowork [options] <rules files> \noptions are:"

let eprint_and_exit msg err_code =  
  eprintf "%s\n%!" msg;
  exit err_code
  
let files : string list ref = ref []

let file_argument name =
  if Filename.check_suffix name Config.rule_suffix
  then
    files := name :: !files
  else
    eprintf "[Warning] Unknown file type : %s. Argument ignored\n" name

let find_file fname =
  let is_file_in_dir dir =
    Sys.file_exists (dir ^ "/" ^ fname)
  in
  try 
    List.find is_file_in_dir (Config.get_path ())
    ^ "/" ^ fname
  with Not_found -> 
    eprint_and_exit 
      (sprintf "[Error] Cannot find the file %s\n%!" fname) 1


let process_file system fname =
  let fpath =
    if Filename.is_implicit fname then
      find_file fname
    else if Sys.file_exists fname then
      fname
    else 
      eprint_and_exit (sprintf "[Error] Cannot find the file %s\n%!" fname) 1 in
  begin
    if !Config.verbose then 
      printf "Evaluating file %s...\n%!" fname;
    let ic = open_in fpath in
    try
      let ast = Parser_include.parse_nowork_file ic in
      let new_system = List.fold_left Eval.evaluate_structure_item system ast in
      (* Pretty.print_system Format.std_formatter new_system; *)
      close_in ic;
      (* todo type check. before or after ? *)
      new_system
    with
    | e ->
      Display_test.print_unknown_exc e (sprintf "processing of the file %s" fname);
      close_in ic;
      system
  end

let main k =
  let open Symbols in
  begin
    (* Parse the command-line *)
    Arg.parse Config.list file_argument usage;

    (if !Config.debug then
      Printexc.record_backtrace true);

    (* Parse & Eval the files *)
    let system = 
      if !Config.reset_system then
      begin
        List.iter (fun fname -> ignore (process_file empty_system fname))
          (List.rev !files);
        empty_system
      end
      else
        List.fold_left (fun acc fname -> process_file acc fname) empty_system
          (List.rev !files) in
    if not !Config.no_repl then
      (* Continuation, might be a 'Read-Eval-Print-Loop' *)
      k system
  end
