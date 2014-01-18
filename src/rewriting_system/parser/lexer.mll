(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Matthieu Dien
*)

{
  open Hashtbl
  open Parser

  let (>>) f h = h f

  let keyword_table = Hashtbl.create 8
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [ "kind", KIND;
	"type", TYPE;
	"atom", ATOM;
	"operator", OPERATOR;
	"constant", CONSTANT;
	"rule", RULE;
	"include", INCLUDE;
	"forall", FORALL;
      ]

  let comments_level = ref 0

}

let word = ['a'-'z''A'-'Z''0'-'9']['-''a'-'z''A'-'Z''0'-'9']*
let placeholder = ['?']['-''a'-'z''A'-'Z''0'-'9']*
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
let arrow = "->"
let doublearrow = "=>"
let star = '*'
let comma = ','
let any = '_'
let space = ['\t' ' ']*
let newline = ['\n' '\r']
let comment = '#' [^ '\n' '\r' ] *
let begin_comment = "(*"
let end_comment = "*)"

  rule token = parse
    | space
	{token lexbuf}
    | begin_comment
	{ comments_level := 1 ;
	  ignore(comment lexbuf) ;
	  token lexbuf }
    | end_comment { failwith "Comment already closed"; }
    | comment
	{ token lexbuf }
    | word as s
	{ try
	    Hashtbl.find keyword_table s
	  with Not_found ->
	      WORD(s) }
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
    | colon { COLON }
    | arrow { ARROW }
    | doublearrow { DARROW }
    | star { STAR }
    | comma { COMMA }
    | any { ANY }
    | newline
	{ Lexing.new_line lexbuf;
	  NEWLINE
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
