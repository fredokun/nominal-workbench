%{
(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright NoWork team
*)

open Parsing
open Lexing
open Rewriting_ast
open Term_ast
open Strategy_ast
open Parsing_ast
open Hashtbl
open Sys

let create_decl (name : string) (desc : rewriting_desc) : rewriting_decl =
  {
    name = name;
    info = Parsing.symbol_start_pos ();
    desc = desc;
  }

let create_term (name : string) (desc : term_desc) : term_ast =
  {
    name = name;
    info = Parsing.symbol_start_pos ();
    desc = desc;
  }

%}

/* values */
%token <int> NUM
%token <string> LIDENT UIDENT PLACEHOLDER STRING

/* keywords */
%token KIND TYPE ATOM OPERATOR RULE CONSTANT OPEN FORALL REWRITE WITH LET
%token STRATEGY REC PROJ

/* interactive commands */
%token LOAD_TEST FAILWITH MATCH HELP QUIT TEST
%token IN_CMD_OPTION EQUAL_CMD_OPTION WITH_CMD_OPTION
%token CTYPE WITH_TYPE_OPTION DOT

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET LACCOL RACCOL SEMICOL
%token SEMICOLSEMICOL COLON EQUAL ARROW
%token DARROW STAR COMMA LT GT DOT ANY SEITHER PLUS

/* comments */
%token EOF

%start start
%type <Parsing_ast.structure> start

%start toplevel_phrase
%type <Parsing_ast.structure> toplevel_phrase

%right STAR DARROW ARROW COLON SEMICOL DOT EITHER

%%

start:
| decls EOF { $1 }
| EOF { raise End_of_file }

toplevel_phrase:
| decls SEMICOLSEMICOL { $1 }
| EOF { raise End_of_file }
;

decls:
| decl decls
    { $1::$2}
| decl
    { [$1] }


decl:
| interactive_command { PInteractiveCmd $1 }
| kind_decl { PDecl $1 }
| constant_decl { PDecl $1 }
| operator_decl { PDecl $1 }
| rule_decl { PDecl $1 }
| strategy_decl { PDecl $1 }
| term_expr { PTermExpr $1 }
| OPEN STRING { PFile_include $2 }
| OPEN UIDENT { PFile_include $2 }
| OPEN LIDENT { PFile_include $2 }

/* | OPEN STRING
    { match Include.nw_include files_included include_paths $2 with
      | None -> (None, None)
      | Some(f) ->(None, Some f)
    }
*/

/* Top-level commands */

interactive_command:
| LOAD_TEST STRING expectation { LoadTest ($2, $3) }
| TEST test_term_predicate { TermTest(TMustPass($2)) }
| TEST term_expr FAILWITH domain_error { TermTest(TMustFail($2, $4)) }
| MATCH term_expr WITH_CMD_OPTION rule_side_pattern { TermMatch($2, $4) }
| MATCH term_expr WITH_TYPE_OPTION type_binders operator_type { TermMatchType($2, $4, $5) }
| MATCH term_expr WITH_TYPE_OPTION operator_type { TermMatchType($2, [], $4) }
| DOT term_expr STRING { TermToDot($2, $3) }
| CTYPE term_expr { TermType $2 }
| QUIT { Quit }

test_term_predicate:
| term_expr IN_CMD_OPTION term_expr_list { InPredicate($1, $3) }
| term_expr EQUAL_CMD_OPTION term_expr_list { EqualPredicate($1, $3) }

expectation:
| FAILWITH domain_error { MustFail ($2) }
| { MustPass }

domain_error:
| UIDENT DOT UIDENT { Error($1, $3) }

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
| CONSTANT NUM COLON type_binders constant_type  /* TODO : factorize */
    { create_decl (string_of_int $2) (DConstant ($4, $5)) }
| CONSTANT NUM COLON constant_type
    { create_decl (string_of_int $2) (DConstant ([],$4)) }

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
| NUM { PConstant (string_of_int $1) }
| UIDENT { PConstant $1 }
| UIDENT LPAREN rule_side_list_pattern RPAREN { POperator ($1, $3) }
| PLACEHOLDER { PPlaceholder $1 }
| ANY { PAny }

rule_side_list_pattern:
| rule_side_pattern COMMA rule_side_list_pattern { $1 :: $3 }
| rule_side_pattern { [$1] }

rule_side_effect:
| NUM { EConstant (string_of_int $1) }
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

strategy_head :
| STRATEGY UIDENT COLON { ($2, []) }
| STRATEGY UIDENT LPAREN strategy_param_list RPAREN COLON { ($2, $4) }

strategy_param_list :
| LIDENT { [$1] }
| LIDENT COMMA strategy_param_list { $1 :: $3 }

strategy_expression :
| LPAREN strategy_expression RPAREN { $2 }
| strategy_simple_expression { $1 }
| strategy_operator { $1 }
| strategy_advanced_expression { $1 }

strategy_simple_expression :
| LIDENT LPAREN RPAREN { base_strat_simple $1 }
| LIDENT LPAREN strategy_expression RPAREN { base_strat $1 $3 }

strategy_operator :
| strategy_expression SEITHER strategy_expression { SEither ($1, $3) }
| strategy_expression PLUS strategy_expression { SChoice ($1, $3) }
| strategy_expression SEMICOL strategy_expression { SSeq ($1, $3) }

strategy_advanced_expression :
| LIDENT { SVar $1 }
| RULE LPAREN RPAREN { SRule None }
| RULE LPAREN LIDENT RPAREN { SRule (Some $3) }
| PROJ LPAREN NUM COMMA strategy_expression RPAREN { SProj ($3, $5) }
| LBRACKET LIDENT RBRACKET { SRule (Some $2) }
| UIDENT { SCall ($1, []) }
| UIDENT LPAREN strategy_expression_list RPAREN { SCall ($1, $3) }

strategy_expression_list :
| strategy_expression { [$1] }
| strategy_expression COMMA strategy_expression_list { $1 :: $3 }

/* terms */

term_expr_list :
| term_expr { [$1] }
| term_expr SEMICOL term_expr_list { $1 :: $3 }

term_expr:
| LPAREN term_expr RPAREN { $2 }
| LET LIDENT EQUAL term_expr { PTermLet ($2, $4) }
| REWRITE term_expr WITH strategy_expression { PTermRewrite ($2, $4) }
| term { PTerm $1 }

term:
| UIDENT LPAREN term_params RPAREN { create_term $1 (Term $3) }
| UIDENT { create_term $1 Const }
| NUM { create_term (string_of_int $1) Const }
| LIDENT { create_term $1 Var }

term_params:
| term {  [$1] }
| term COMMA term_params { $1::$3 }

%%
