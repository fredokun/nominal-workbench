{
  open Hashtbl
  open Parser

  let (>>) f h = h f

  let keyword_table = Hashtbl.create 6
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [ "kind", KIND;
	"type", TYPE;
	"atom", ATOM;
	"operator", OPERATOR;
	"constant", CONSTANT;
	"rule", RULE ]

  let comments_counter = ref 0

}

let word = ['a'-'z''A'-'Z']['-''a'-'z''A'-'Z''0'-'9']*
let number = ['0'-'9']* '.'? ['0'-'9']*

let lparen = '('
let rparen = ')'
let lbracket = '['
let rbracket = ']'
let laccol = '{'
let raccol = '}'
let semicol = ';'
let colon = ':'
let arrow = "->"
let doublearrow = "=>"
let qmark = '?'
let star = '*'
let comma = ','
let space = ['\t' ' ']*
let newline = ['\n' '\r']
let comment = '#' [^ '\n' '\r' ] *

  rule token = parse
    | space
	{token lexbuf}
    | comment 
	{ token lexbuf }
    | number as n
	{ NUM(n >> float_of_string) }
    | word as s
	{ try
	    Hashtbl.find keyword_table s
	  with Not_found ->
	      WORD(s) }
    | lparen { LPAREN }
    | rparen { RPAREN }
    | lbracket { LBRACKET }
    | rbracket { RBRACKET }
    | laccol { LACCOL }
    | raccol { RACCOL }
    | semicol { SEMICOL }
    | colon { COLON }
    | arrow { ARROW }
    | doublearrow { DARROW }
    | qmark { QMARK }
    | star { STAR }
    | comma { COMMA }
    | newline
	{ Lexing.new_line lexbuf;
	  NEWLINE
	}
    | eof { EOF }
    | _
	{ failwith ("Unknown symbol " ^ Lexing.lexeme lexbuf) }

{
}
