%{
(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Matthieu Dien
  (C) Copyright Vincent Botbol
*)

  open Parsing
  open Lexing
  open Ast
  open Include

  let err_msg = fun kwd name msg ->
    let pos = Parsing.symbol_start_pos () in
    kwd ^ " \"" ^ name ^"\" " ^ msg ^ " line \
    " ^ (string_of_int (pos.Lexing.pos_lnum)) ^ ", col \
    " ^ (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

  let annote_pos item =
    let pos = Parsing.symbol_start_pos () in
    { value = item ; info = pos }

%}

/* values */
%token <float> NUM
%token <string> WORD PLACEHOLDER

/* keywords */
%token KIND TYPE ATOM OPERATOR RULE CONSTANT INCLUDE FORALL

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET LACCOL RACCOL SEMICOL COLON EQUAL ARROW
%token DARROW STAR COMMA QMARK NEWLINE LT GT DOT

%token EOF

%start start
%type rewriting_ast start

%start toplevel_phrase
%type rewriting_ast toplevel_phrase

%right STAR DARROW ARROW

%%

start:
| decls EOF { }

toplevel_phrase:
| decls SEMICOL SEMICOL { }
| EOF { raise End_of_file }
;

decls :
| decl decls { }
| decl { }

decl:
| kind_decl { }
| constant_decl { }
| operator_decl { }
| rule_decl { }
| INCLUDE WORD { }
| NEWLINE { }


/* kinds */

kind_decl:
| KIND WORD COLON kind_lfth { }

kind_lfth:
| kind_type { }

kind_type:
| kind_type ARROW kind_type { }
| TYPE { }
| ATOM { }


/* generic types for constants and operators */

generic_type:
| FORALL LPAREN WORD RPAREN DOT generic_type { }
| WORD LT generic_type GT { }
| WORD { }
| LBRACKET WORD RBRACKET { }

/* constants */

constant_decl:
| CONSTANT WORD COLON generic_type { }

/* operators */

operator_decl:
| OPERATOR WORD COLON operator_type ARROW operator_type
    { }

operator_type:
| generic_type { }
| generic_type STAR operator_type { }

/* rules */

rule_decl:
| rule_head rule_body { }
| rule_head NEWLINE rule_body { }

rule_head:
| RULE LBRACKET WORD RBRACKET COLON { }

rule_body:
| rule_side DARROW rule_side { }

rule_side:
| WORD { }
| WORD LPAREN RPAREN { }
| WORD LPAREN rule_side_list RPAREN { }
| PLACEHOLDER { }

rule_side_list:
| rule_side COMMA rule_side_list { }
| rule_side { }

%%
