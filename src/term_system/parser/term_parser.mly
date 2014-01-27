%{
(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Matthieu Dien
  (C) Copyright Vincent Botbol
*)

  open Parsing
  open Lexing
  (* open Rewriting_ast *)
  open Term_ast

  let err_msg = fun kwd name msg ->
    let pos = Parsing.symbol_start_pos () in
    kwd ^ " \"" ^ name ^"\" " ^ msg ^ " line \
    " ^ (string_of_int (pos.Lexing.pos_lnum)) ^ ", col \
    " ^ (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

  let annote_pos item : Term_ast.info * Term_ast.term =
    let pos = Parsing.symbol_start_pos () in
    pos, item

%}

/* values */
%token <float> NUM
%token <string> IDENT
%token <string> VARIDENT

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET COMMA SEMICOL LT GT

%token EOF

%start start
%type <Term_ast.term_ast> start

%start toplevel_phrase
%type <Term_ast.term> toplevel_phrase

%%

start:
| expressions EOF { TermAST $1 }

toplevel_phrase:
| expression SEMICOL SEMICOL { $1 }
| EOF { raise End_of_file }

expressions:
| expression { [annote_pos @@ $1] }
| expression expressions { annote_pos $1::$2 }

expression:
| LPAREN expression RPAREN { $2 }
| LBRACKET IDENT RBRACKET { Binder($2) }
| IDENT LPAREN expression_params RPAREN
  { Term($1, [], $3) }
| IDENT { Const($1) }
| VARIDENT { Var($1, None) }

expression_params:
| expression { [$1] }
| expression COMMA expression_params { $1::$3 }

%%
