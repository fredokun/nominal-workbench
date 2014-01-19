(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Vincent Botbol
*)

open Rewriting_ast

let usage = "Usage: nowork [options] <rules files> <terms files>\noptions are:"

let rule_files : string list ref = ref []
let term_files : string list ref = ref []

(* ! *)
let rewriting_ast : rewriting_ast ref = ref (RewritingAST [])

let file_argument name =
  if Filename.check_suffix name Config.rule_suffix then
    rule_files := name :: !rule_files
  else if Filename.check_suffix name Config.term_suffix then
    term_files := name :: !term_files
  else 
    Printf.eprintf "[Warning] Unknown file type : %s. Argument ignored\n" name

let process_rule_files rfile =
  if Sys.file_exists rfile then
    Printf.eprintf "[Warning] %s doesn't exist. Skipping file...\n" rfile
  else 
    begin 
      let ic = open_in rfile in
      let (RewritingAST new_decls, l) = 
	Parser.start Lexer.token (Lexing.from_channel ic) in
      (* ! *)
      rewriting_ast := RewritingAST
	(match !rewriting_ast with
	  | RewritingAST prev_decls -> new_decls@prev_decls);
      close_in ic;
    end

let main () =
  begin
    (* Parse the command-line *)
    Arg.parse Config.list file_argument usage;
    
    (* Parse the rule files *)
    List.iter process_rule_files !rule_files;

    (* Parse the term files *)
    List.iter 
      (fun s -> Printf.printf "%s ignored : term evaluation not yet implemented\n" s)
      !term_files;

    (* Read-Eval-Print-Loop *)
    Toploop.loop Format.std_formatter
  end

let () = main ()
