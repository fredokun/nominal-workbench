(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Matthieu Dien
*)

{
  open Term_parser
}

let ident = ['A'-'Z']['-''a'-'z''A'-'Z''0'-'9']*
let var_ident = ['a'-'z']['-''a'-'z''A'-'Z''0'-'9']*
let number = ['0'-'9']* '.'? ['0'-'9']*

let lparen = '('
let rparen = ')'
let lbracket = '['
let rbracket = ']'
let comma = ','
let space = ' '
let newline = '\n'

  rule token = parse
    | space {token lexbuf}
(*    | number as n
	{ NUM(n >> float_of_string) } *)
    | ident as s { IDENT(s) }
    | var_ident as s { VARIDENT(s) }
    | lparen { LPAREN }
    | rparen { RPAREN }
    | lbracket { LBRACKET }
    | rbracket { RBRACKET }
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
