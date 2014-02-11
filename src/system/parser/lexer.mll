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
      ; "reduce", REDUCE
      ; "with", WITH
      ; "term", TERM
      ]

  let directive_table = Hashtbl.create 16
  let () =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [ ":help", HELP
      ; 
      ]

}

let lower_ident = ['a'-'z']['-''a'-'z''A'-'Z''0'-'9']*
let upper_ident = ['A'-'Z''0'-'9']['-''a'-'z''A'-'Z''0'-'9']*
let placeholder = '?' ['-''a'-'z''A'-'Z''0'-'9'] +
let filename = ['a'-'z''A'-'Z''0'-'9']['/''-''_''.''a'-'z''A'-'Z''0'-'9']*

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

let directive = colon lower_ident

  rule token = parse
    | space
	{token lexbuf}
    | comment
	{ token lexbuf }
    | lower_ident as s
	{ try
	    Hashtbl.find keyword_table s
	  with Not_found ->
	    LIDENT(s) }
    | directive as s
	{ try
	    Hashtbl.find directive_table s
	  with Not_found ->
	    assert false (* todo *)
	}
    | upper_ident as s
	{ UIDENT(s) }
    | filename as f { FILENAME(f) }
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
    | any { ANY }
    | newline
	{ Lexing.new_line lexbuf;
	  token lexbuf
	}
    | eof { EOF }
    | _
	{ failwith ("Unknown symbol " ^ Lexing.lexeme lexbuf) }


{
}
