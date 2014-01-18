%{
(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Matthieu Dien
  (C) Copyright Vincent Botbol
*)

  open Parsing
  open Lexing
  open Rewriting_ast
  open Include

  (* let annote_pos item = *)
  (*   let pos = Parsing.symbol_start_pos () in *)
  (*   { value = item ; info = pos } *)

  let get_info x =
    Parsing.symbol_start_pos ()

  let parse_error s =
    let pos = Parsing.symbol_start_pos () in
    let msg = "Parsing error line " ^ (string_of_int (pos.Lexing.pos_lnum)) ^ ", col \
    " ^ (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)) in
    print_endline msg

%}

/* values */
%token <float> NUM
%token <string> WORD PLACEHOLDER FILENAME

/* keywords */
%token KIND TYPE ATOM OPERATOR RULE CONSTANT INCLUDE FORALL

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET LACCOL RACCOL SEMICOL COLON EQUAL ARROW
%token DARROW STAR COMMA NEWLINE LT GT DOT ANY

%token EOF

%start start
%type <Rewriting_ast.rewriting_ast * string list> start

%start toplevel_phrase
%type <Rewriting_ast.rewriting_ast * string list> toplevel_phrase

%right STAR DARROW ARROW COLON DOT

%%

start:
| decls EOF { let (ast, includes) = $1 in (RewritingAST ast, includes) }

toplevel_phrase:
| decls SEMICOL SEMICOL { let (ast, includes) = $1 in (RewritingAST ast, includes) }
| EOF { raise End_of_file }
;

decls :
| decl decls
    { match $1 with
      | (None, None) -> $2
      | (None, Some(f)) -> let (ast, includes) = $2 in (ast, f::includes)
      | (Some(a), None) -> let (ast, includes) = $2 in (a::ast, includes)
      | (Some(a), Some(f)) -> let (ast, includes) = $2 in (a::ast, f::includes)
    }
| decl
   { match $1 with
    | (None, None) -> ([],[])
    | (None, Some(f)) -> ([], [f])
    | (Some(a), None) -> ([a], [])
    | (Some(a), Some(f)) -> ([a], [f])
   }

decl:
| kind_decl { (Some $1, None) }
| constant_decl { (Some $1, None) }
| operator_decl { (Some $1, None) }
| rule_decl { (Some $1, None) }
| INCLUDE FILENAME
    { match Include.nw_include $2 with
      | None -> (None, None)
      | Some(f) ->(None, Some f)
    }
| NEWLINE { (None, None) }


/* kinds */

kind_decl:
| KIND WORD COLON kind_type { ($2, get_info (), (DKind (Kind $4))) }

kind_type:
| kind_type DARROW kind_type { $1 @ $3 }
| TYPE { [Type] }
| ATOM { [Atom] }

/* constants */

constant_decl:
| CONSTANT WORD COLON type_binders constant_type
    { ($2, get_info (), DConstant (Constant ($4, $5))) }
| CONSTANT WORD COLON constant_type
    { ($2, get_info (), DConstant (Constant ([],$4)))}

type_binders:
| FORALL LPAREN word_list RPAREN DOT { $3 }
| FORALL LPAREN word_list RPAREN DOT type_binders { $3 @ $6 }

constant_type:
| WORD application_constant_type_list { TypeApplication ($1, $2) }
| WORD { TypeName $1 }

application_constant_type_list:
| LT constant_type_list GT { $2 }
| LT constant_type_list GT application_constant_type_list { $2 @ $4 }

constant_type_list:
| constant_type COMMA constant_type_list { $1 :: $3 }
| constant_type { [$1] }

word_list:
| WORD COMMA word_list { $1 :: $3 }
| WORD { [$1] }


/* operators */

operator_decl:
| OPERATOR WORD COLON type_binders operator_type ARROW operator_without_binder_type 
    { ($2, get_info (), DOperator (Operator ($4, $5, $7))) }
| OPERATOR WORD COLON operator_type ARROW operator_without_binder_type
    { ($2, get_info (), DOperator (Operator ([], $4, $6))) }

operator_type:
| operator_without_binder_type { [OpTypeArg $1] }
| LBRACKET WORD RBRACKET DOT operator_type { (OpBinderArg $2) :: $5 }
| operator_type STAR operator_type { $1 @ $3 }

operator_without_binder_type:
| WORD application_operator_without_binder_type_list { TypeApplication ($1, $2) }
| WORD { TypeName $1 }

application_operator_without_binder_type_list:
| LT operator_without_binder_type_list GT { $2 }
| LT operator_without_binder_type_list GT application_operator_without_binder_type_list { $2 @ $4 }

operator_without_binder_type_list:
| operator_without_binder_type COMMA operator_without_binder_type_list { $1 :: $3 }
| operator_without_binder_type { [$1] }




/* rules */

rule_decl:
| rule_head rule_body { ($1, get_info (), DRule $2) }
| rule_head NEWLINE rule_body { ($1, get_info (), DRule $3) }

rule_head:
| RULE LBRACKET WORD RBRACKET COLON { $3 }

rule_body:
| rule_side_pattern DARROW rule_side_effect { Rule ($1,$3)  }

rule_side_pattern:
| WORD { PConstant $1 }
| WORD LPAREN rule_side_list_pattern RPAREN { POperator ($1, $3) }
| PLACEHOLDER { PPlaceholder $1 }
| ANY { PAny }


rule_side_list_pattern:
| rule_side_pattern COMMA rule_side_list_pattern { $1 :: $3 }
| rule_side_pattern { [$1] }

rule_side_effect:
| WORD { EConstant $1 }
| WORD LPAREN rule_side_list_effect RPAREN { EOperator ($1, $3) }
| PLACEHOLDER { EPlaceholder $1 }

rule_side_list_effect:
| rule_side_effect COMMA rule_side_list_effect { $1 :: $3 }
| rule_side_effect { [$1] }


%%
