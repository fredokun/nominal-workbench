(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Vincent Botbol
*)

let initialize_toplevel_env () =
  ()


let process_term rules t =
  let open Term_ast in
  try
    let nt = Rewriting.rewrite rules t in
    Printf.printf "Term : %s rewrote into %s\n%!" 
      (string_of_term t)
      (string_of_term nt)
  with 
  | _ -> 
    Printf.eprintf "Unhandled Term error : %s\n%!" (string_of_term t)


let execute_phrase print_outcome ppf phr =
  (* tmp *)
  let rules = Symbols.list_of_rules Symbols.empty_system in
  process_term rules phr;
  ()

let first_line = ref true
let got_eof = ref false

let read_input prompt buffer len =
  Printf.printf "%s%!" prompt;
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = input_char Pervasives.stdin in
      buffer.[!i] <- c;
      incr i;
      if c = '\n' then raise Exit;
    done;
    (!i, false)
  with
    | End_of_file ->
      (!i, true)
    | Exit ->
      (!i, false)

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !first_line then "# "
      else "  "
    in
    first_line := false;
    let (len, eof) = read_input prompt buffer len in
    if eof then begin
      print_newline ();
      if len > 0 then got_eof := true;
      len
    end else
      len
  end

let parse_toplevel_rule phr =
  let ast = Parser.toplevel_phrase Lexer.token phr in
  Parsing.clear_parser ();
  ast

let parse_toplevel_term phr = 
  let term = Term_parser.toplevel_phrase Term_lexer.token phr in
  if !Config.verbose then
    Printf.printf "%s\n%!" (Term_ast.string_of_term term);
  Parsing.clear_parser ();
  term
    
let loop ppf =
  Format.fprintf ppf "        NoWork version %s@.@." "0.00.1" (* Config.version *);
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Sys.catch_break true;
  while true do
    try
      Lexing.flush_input lb;
      first_line := true;
      (* TODO : discriminer les termes, des règles.. combinaison des parsers? *)
      let phr = parse_toplevel_term lb in
      ignore (execute_phrase true ppf phr)
    with
      | End_of_file -> exit 0
      | Sys.Break -> Format.fprintf ppf "Interrupted.@."
      | x ->
	Format.fprintf ppf "%s.@." (Printexc.to_string x)
  (* Todo : handle errors *)
  done
