(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

{
  open Term_parser

  let comments_level = ref 0
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
let newline = [ '\n' '\r' ]
let semicol = ';'

let comment = '#' [^ '\n' '\r' ]*
let begin_comment = "(*"
let end_comment = "*)"

  rule token = parse
    | space {token lexbuf}
    | comment { token lexbuf }
    | begin_comment
	{ comments_level := 1 ;
	  ignore(comment lexbuf) ;
	  token lexbuf }
    | end_comment { failwith "Comment already closed"; }

    (*  | number as n
	  { NUM(n >> float_of_string) } *)
    | ident as s { IDENT(s) }
    | var_ident as s { VARIDENT(s) }
    | lparen { LPAREN }
    | rparen { RPAREN }
    | lbracket { LBRACKET }
    | rbracket { RBRACKET }
    | comma { COMMA }
    | semicol { SEMICOL }
    | newline
	{ Lexing.new_line lexbuf;
	  token lexbuf
	}
    | eof { EOF }
    | _
	{ failwith ("Unknown symbol " ^ Lexing.lexeme lexbuf) }

  and comment = parse
    | begin_comment
	{ incr comments_level;
	  comment lexbuf }
    | end_comment
	{ decr comments_level;
	  match !comments_level with
	    | 0 -> token lexbuf
	    | n when n > 0 -> comment lexbuf
	    | _ -> failwith "Comment already closed"
	}
    | newline { Lexing.new_line lexbuf; comment lexbuf }
    | eof { if !comments_level != 0
      then failwith "Close your comment"
      else token lexbuf }
    | _ { comment lexbuf }

{
}
