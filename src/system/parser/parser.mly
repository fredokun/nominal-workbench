%{
(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Matthieu Dien
  (C) Copyright Vincent Botbol
*)

open Parsing
open Lexing
open Rewriting_ast
open Term_ast
open Strategy_ast
open Parsetree
open Include
open Hashtbl
open Sys

(* let annote_pos item = *)
(*   let pos = Parsing.symbol_start_pos () in *)
(*   { value = item ; info = pos } *)

let annote_term item : Term_ast.info * Term_ast.term =
  let pos = Parsing.symbol_start_pos () in
  pos, item

let parse_error s =
  let pos = Parsing.symbol_start_pos () in
  let msg = "Parsing error line " ^ (string_of_int (pos.Lexing.pos_lnum)) ^ ", col \
  " ^ (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)) in
  print_endline msg

let create_decl name desc =
  {
    name = name;
    info = Parsing.symbol_start_pos ();
    desc = desc;
  }

let create_strat name content =
  match name with
  | "test" -> STest content
  | "not" -> SNot content
  | "all" -> SAll content
  | _ -> assert false

let create_strat_simple = function
  | "id" -> SId
  | "fail" -> SFail
  | _ -> assert false

%}

/* values */
%token <float> NUM
%token <string> LIDENT UIDENT PLACEHOLDER FILENAME

/* keywords */
%token KIND TYPE ATOM OPERATOR RULE CONSTANT OPEN FORALL REDUCE WITH TERM 
%token STRATEGY REC

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET LACCOL RACCOL SEMICOL COLON EQUAL ARROW
%token DARROW STAR COMMA LT GT DOT ANY SEITHER

/* directives */
%token HELP

/* comments */
%token EOF

%start start
%type <Parsetree.structure> start

%start toplevel_phrase
%type <Parsetree.structure_item> toplevel_phrase

%right STAR DARROW ARROW COLON DOT EITHER

%%

start:
| decls EOF { $1 }

toplevel_phrase:
| decl SEMICOL SEMICOL { $1 }
| EOF { raise End_of_file }
;

decls:
| decl decls
    { $1::$2}
| decl
    { [$1] }


decl:
| kind_decl { PDecl $1 }
| constant_decl { PDecl $1 }
| operator_decl { PDecl $1 }
| rule_decl { PDecl $1 }
| strategy_decl { PDecl $1 }
| TERM LIDENT EQUAL term { PTermDecl ($2, $4) }
| REDUCE term WITH strategy_expression { PReduce ($2, $4) }
| OPEN FILENAME { PFile_include $2 }
| OPEN UIDENT { PFile_include $2 }
| OPEN LIDENT { PFile_include $2 }
| term { PTerm $1 }
/* | OPEN FILENAME
    { match Include.nw_include files_included include_paths $2 with
      | None -> (None, None)
      | Some(f) ->(None, Some f)
    }
*/

/* kinds */

kind_decl:
| KIND UIDENT COLON kind_type { create_decl $2 (DKind $4) }

kind_type:
| kind_type ARROW kind_type { $1 @ $3 }
| TYPE { [Type] }
| ATOM { [Atom] }

/* constants */

constant_decl:
| CONSTANT UIDENT COLON type_binders constant_type
    { create_decl $2 (DConstant ($4, $5)) }
| CONSTANT UIDENT COLON constant_type
    { create_decl $2 (DConstant ([],$4)) }

type_binders:
| FORALL LPAREN word_list RPAREN DOT { $3 }
| FORALL LPAREN word_list RPAREN DOT type_binders { $3 @ $6 }

constant_type:
| UIDENT application_constant_type_list { TypeApplication ($1, $2) }
| UIDENT { TypeName $1 }

application_constant_type_list:
| LT constant_type_list GT { $2 }
| LT constant_type_list GT application_constant_type_list { $2 @ $4 }

constant_type_list:
| constant_type COMMA constant_type_list { $1 :: $3 }
| constant_type { [$1] }

word_list:
| UIDENT COMMA word_list { $1 :: $3 }
| UIDENT { [$1] }
| LIDENT COMMA word_list { $1 :: $3 }
| LIDENT { [$1] }

/* operators */

operator_decl:
| OPERATOR UIDENT COLON type_binders operator_type ARROW operator_without_binder_type 
    { create_decl $2 (DOperator ($4, $5, $7)) }
| OPERATOR UIDENT COLON operator_type ARROW operator_without_binder_type
    { create_decl $2 (DOperator ([], $4, $6)) }

operator_type:
| operator_without_binder_type { [OpTypeArg $1] }
| LBRACKET UIDENT RBRACKET DOT operator_type { (OpBinderArg $2) :: $5 }
| operator_type STAR operator_type { $1 @ $3 }

operator_without_binder_type:
| UIDENT application_operator_without_binder_type_list { TypeApplication ($1, $2) }
| UIDENT { TypeName $1 }

application_operator_without_binder_type_list:
| LT operator_without_binder_type_list GT { $2 }
| LT operator_without_binder_type_list GT application_operator_without_binder_type_list { $2 @ $4 }

operator_without_binder_type_list:
| operator_without_binder_type COMMA operator_without_binder_type_list { $1 :: $3 }
| operator_without_binder_type { [$1] }


/* rules */

rule_decl:
| rule_head rule_body { create_decl $1 (DRule $2) }

rule_head:
| RULE LBRACKET LIDENT RBRACKET COLON { $3 }
| RULE LBRACKET UIDENT RBRACKET COLON { $3 }

rule_body:
| rule_side_pattern DARROW rule_side_effect { $1,$3 }

rule_side_pattern:
| UIDENT { PConstant $1 }
| UIDENT LPAREN rule_side_list_pattern RPAREN { POperator ($1, $3) }
| PLACEHOLDER { PPlaceholder $1 }
| ANY { PAny }


rule_side_list_pattern:
| rule_side_pattern COMMA rule_side_list_pattern { $1 :: $3 }
| rule_side_pattern { [$1] }

rule_side_effect:
| UIDENT { EConstant $1 }
| UIDENT LPAREN rule_side_list_effect RPAREN { EOperator ($1, $3) }
| PLACEHOLDER { EPlaceholder $1 }

rule_side_list_effect:
| rule_side_effect COMMA rule_side_list_effect { $1 :: $3 }
| rule_side_effect { [$1] }

/* strategies */

strategy_decl:
| strategy_head strategy_expression 
  { 
    let name, signature = $1 in
    create_decl name (DStrategy (signature, $2))
  }

strategy_head:
| STRATEGY LIDENT COLON { ($2, []) }

strategy_expression :
| LPAREN strategy_expression RPAREN { $2 }
| strategy_simple_expression { $1 }
| strategy_operator { $1 }
| strategy_advanced_expression { $1 }

strategy_simple_expression :
| LIDENT LPAREN RPAREN { create_strat_simple $1 }
| LIDENT LPAREN strategy_expression RPAREN { create_strat $1 $3 }

strategy_operator :
| strategy_expression SEITHER strategy_expression { SEither ($1, $3) }
| strategy_expression SEMICOL strategy_expression { SSeq ($1, $3) }

strategy_advanced_expression :
| LIDENT { SVar $1 }
| REC LPAREN LIDENT COMMA strategy_expression RPAREN { SRec ($3, $5) }
| RULE LPAREN RPAREN { SRule None }
| RULE LPAREN UIDENT RPAREN { SRule (Some $3) } 
| UIDENT LPAREN RPAREN { SCall ($1, []) }
| UIDENT LPAREN strategy_expression_list RPAREN { SCall ($1, $3) }

strategy_expression_list :
| strategy_expression { [$1] }
| strategy_expression COMMA strategy_expression_list { $1 :: $3 }

/* terms */

term:
| LPAREN term RPAREN { $2 }
| UIDENT LPAREN term_params RPAREN
    { Term($1, $3) }
| UIDENT { Const($1) }
| LIDENT { Var($1) }

term_params:
| term {  [$1] }
| term COMMA term_params { $1::$3 }
    
%%
