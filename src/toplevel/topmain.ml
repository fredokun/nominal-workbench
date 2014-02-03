(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Vincent Botbol
*)

open Rewriting_ast

let usage = "Usage: nowork [options] <rules files> <terms files>\noptions are:"

let rule_files : string list ref = ref []
let term_files : string list ref = ref []

let file_argument name =
  if Filename.check_suffix name Config.rule_suffix then
    rule_files := name :: !rule_files
  else if Filename.check_suffix name Config.term_suffix then
    term_files := name :: !term_files
  else
    Printf.eprintf "[Warning] Unknown file type : %s. Argument ignored\n" name

let process_rule_file rfile =
  if not (Sys.file_exists rfile) then
    Printf.eprintf "[Warning] %s doesn't exist. Skipping file...\n%!" rfile
  else
    begin
      Printf.printf "Adding rules from : %s...\n%!" rfile;
      let ic = open_in rfile in
      try
        let (ast, _) = Parser.start Lexer.token (Lexing.from_channel ic) in
        let new_system = Symbols.enter_ast !Toploop.system ast in
	  Toploop.system := new_system;
          Type_checking.check_ast new_system ast;
          close_in ic
      with
      | _ ->
        Printf.eprintf "[Warning] Unhandled error. Skipping %s\n%!" rfile;
        close_in ic
    end

let process_term_file rfile =
  if not (Sys.file_exists rfile) then
    Printf.eprintf "[Warning] %s doesn't exist. Skipping file...\n%!" rfile
  else
    begin
      Printf.printf "Evaluating terms from : %s...\n%!" rfile;
      let ic = open_in rfile in
      try
        let (Term_ast.TermAst terms) =
          Term_parser.start Term_lexer.token (Lexing.from_channel ic) in
	(* tmp fix to compile ;) *)
        let rules = Symbols.list_of_rules Symbols.empty_system in
        List.iter (fun (_, t) -> Toploop.process_term rules t) terms;
        close_in ic
      with
      | e ->
        Printf.eprintf "[Warning] Unhandled error : %s. Skipping %s\n%!"
          (Printexc.to_string e) rfile;
        close_in ic
    end

let main () =
  begin
    (* Parse the command-line *)
    Arg.parse Config.list file_argument usage;

    (* Parse the rule files *)
    List.iter process_rule_file !rule_files;

    (* Parse the terms files *)
    List.iter process_term_file !term_files;

    (* Read-Eval-Print-Loop *)
    Toploop.loop Format.std_formatter
  end

let () = main ()
