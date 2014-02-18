(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Matthieu Dien
*)

{
  open Hashtbl
  open Parser
  open Rewriting_parsing_error
  open Utils
  open Parsing

  let (>>) f h = h f

  let str_buff = Buffer.create 100

  let keyword_table = Hashtbl.create 16
  let () =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [ "kind", KIND
      ; "type", TYPE
      ; "atom", ATOM
      ; "operator", OPERATOR
      ; "constant", CONSTANT
      ; "rule", RULE
      ; "open", OPEN
      ; "forall", FORALL
      ; "rewrite", REWRITE
      ; "with", WITH
      ; "let", LET
      ; "strategy", STRATEGY
      ; "rec", REC
      ; "proj", PROJ
      ]

  let directive_table = Hashtbl.create 16
  let () =
    List.iter (fun (kwd, tok) -> Hashtbl.add directive_table kwd tok)
      [ ":help", HELP
      ; ":?", HELP
      ; ":quit", QUIT
      ; ":exit", QUIT
      ; ":q", QUIT
      ; ":load-test", LOAD_TEST
      ; ":test", TEST
      ]

  let directive_option_table = Hashtbl.create 16
  let () =
    List.iter (fun (kwd, tok) -> Hashtbl.add directive_option_table kwd tok)
      [ "--failwith", FAILWITH
      ; "--in", IN_CMD_OPTION
      ; "--equal", EQUAL_CMD_OPTION
      ]
}

let lower_ident = ['a'-'z']['-''a'-'z''A'-'Z''0'-'9']*
let upper_ident = ['A'-'Z']['-' '_' 'a'-'z''A'-'Z''0'-'9']*
let placeholder = '?' ['-''a'-'z''A'-'Z''0'-'9'] +
let num = ['0' - '9'] +

let double_quote = '"'
let not_double_quote = [^ '"' ]
let lparen = '('
let rparen = ')'
let lbracket = '['
let rbracket = ']'
let laccol = '{'
let raccol = '}'
let lt = '<'
let gt = '>'
let dot = '.'
let semicol = ';'
let colon = ':'
let equal = '='
let arrow = "->"
let doublearrow = "=>"
let star = '*'
let comma = ','
let any = '_'
let space = ['\t' ' ']*
let newline = ['\n' '\r']
let comment = '#' [^ '\n' '\r' ] *
let dash = '-'
let either = "+>"

let directive = colon lower_ident
let directive_opt = dash dash lower_ident

  rule token = parse
  | space {token lexbuf}
  | comment { token lexbuf }
  | lower_ident as s {
    try
      Hashtbl.find keyword_table s
    with Not_found ->
      LIDENT(s)
  }
  | directive as s {
    try
      Hashtbl.find directive_table s
    with Not_found ->
      assert false (* todo *)
  }
  | directive_opt as s {
    try
      Hashtbl.find directive_option_table s
    with Not_found ->
      failwith @@ "Unknown directive : " ^ s (* TODO *)
  }
  | upper_ident as s { UIDENT(s) }
  | placeholder as p { PLACEHOLDER(p) }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | lbracket { LBRACKET }
  | rbracket { RBRACKET }
  | laccol { LACCOL }
  | raccol { RACCOL }
  | lt { LT }
  | gt { GT }
  | dot { DOT }
  | semicol { SEMICOL }
  | equal { EQUAL }
  | colon { COLON }
  | arrow { ARROW }
  | doublearrow { DARROW }
  | star { STAR }
  | comma { COMMA }
  | either { SEITHER }
  | num as i { NUM (int_of_string i) }
  | any { ANY }
  | newline { 
    Lexing.new_line lexbuf;
    token lexbuf }
  | double_quote { 
    Buffer.reset str_buff; 
    string lexbuf }
  | eof { EOF }
  | _ { failwith ("Unknown symbol " ^ Lexing.lexeme lexbuf) }

and string = parse
  | not_double_quote as c { 
    Buffer.add_char str_buff c; 
    string lexbuf }
  | double_quote { STRING (Buffer.contents str_buff) } 
  | _ { failwith ("Unfinished string") }  (* TODO actual exception *)
{
}
