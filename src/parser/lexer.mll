(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Matthieu Dien
*)

{
  open Hashtbl
  open Parser

  let (>>) f h = h f

  let keyword_table = Hashtbl.create 7
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [ "kind", KIND;
	"type", TYPE;
	"atom", ATOM;
	"operator", OPERATOR;
	"constant", CONSTANT;
	"rule", RULE;
	"include", INCLUDE;
      ]

  let comments_level = ref 0

}

let word = ['a'-'z''A'-'Z']['-''a'-'z''A'-'Z''0'-'9']*
let number = ['0'-'9']* '.'? ['0'-'9']*
let filename = ['a'-'z''A'-'Z''0'-'9']['/''-''_''.''a'-'z''A'-'Z''0'-'9']*"{.nw}"

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
(*    | number as n
	{ NUM(n >> float_of_string) } *)
    | word as s
	{ try
	    Hashtbl.find keyword_table s
	  with Not_found ->
	      WORD(s) }
    | filename as s
	{ FILENAME(s) }
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
