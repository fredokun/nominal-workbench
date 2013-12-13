(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Vincent Botbol
*)

let initialize_toplevel_env () =
  ()

let execute_phrase print_outcome ppf phr =
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

let parse_toplevel_phrase phr = 
  let ast = Parser.toplevel_phrase Lexer.token phr in
  Parsing.clear_parser ();
  ast

let loop ppf =
  Format.fprintf ppf "        NoWork version %s@.@." "0.00.1" (* Config.version *);
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Sys.catch_break true;
  while true do
    try
      Lexing.flush_input lb;
      first_line := true;
      let phr = parse_toplevel_phrase lb in
      ignore (execute_phrase true ppf phr)
    with
    | End_of_file -> exit 0
    | Sys.Break -> Format.fprintf ppf "Interrupted.@."
    | x -> 
      Format.fprintf ppf "%s.@." (Printexc.to_string x)
      (* Todo : handle errors *)
      
  done
