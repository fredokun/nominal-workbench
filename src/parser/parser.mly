%{
  open Hashtbl

  let operator_tbl = Hashtbl.create 10
  let constant_tbl = Hashtbl.create 10
  let kind_tbl = Hashtbl.create 10
  let rule_tbl = Hashtbl.create 10
  let result = ref 0

%}

/* values */
%token <float> NUM
%token <string> WORD

/* keywords */
%token KIND TYPE ATOM OPERATOR RULE CONSTANT

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET LACCOL RACCOL SEMICOL COLON EQUAL ARROW
%token DARROW STAR COMMA QMARK

%token EOF

%start start
%type <unit> start
%type <unit> kind_decl

%%

start:
| decls EOF
    { if !result = 0 then
	print_endline "Success !"
      else
	print_endline "Fail !"
    }

decls :
| decl decls {}
| decl {}

decl:
| kind_decl {}
| operator_decl {}
| constant_decl {}
| rule_decl {}


/* kinds */

kind_decl:
| KIND WORD COLON kind_lfth
    { try
	prerr_endline (Hashtbl.find kind_tbl $2);
	result := 1
      with Not_found ->
	Hashtbl.add kind_tbl $2 ("Kind " ^ $2 ^ "already defined")
    }

kind_lfth:
| kind_type {}
| ATOM {}

kind_type:
| kind_type STAR kind_type {}
| kind_type DARROW kind_type {}
| TYPE {}


/* constants */

constant_decl:
| CONSTANT WORD COLON WORD
    { try
	prerr_endline (Hashtbl.find constant_tbl $2);
	result := 1
      with Not_found ->
	Hashtbl.add constant_tbl $2 ("Constant \"" ^ $2 ^ "\" already defined")
    }


/* operators */

operator_decl:
| OPERATOR WORD COLON operator_type
    { try
	prerr_endline (Hashtbl.find operator_tbl $2);
	result := 1
      with Not_found ->
	Hashtbl.add operator_tbl $2 ("Operator \"" ^ $2 ^ "\" already defined")
    }

operator_type:
| WORD
    { try
	ignore (Hashtbl.find kind_tbl $1)
      with Not_found ->
	result := 1;
	prerr_endline ("Kind \"" ^ $1 ^ "\" unknown")
    }
| LBRACKET WORD RBRACKET
    { try
	ignore (Hashtbl.find kind_tbl $2)
      with Not_found ->
	result := 1;
	prerr_endline ("Kind \"" ^ $2 ^ "\" unknown")
    }
| operator_type STAR operator_type {}
| operator_type ARROW operator_type {}


/* rules */

rule_decl:
| rule_head rule_body {}

rule_head:
| RULE LBRACKET WORD RBRACKET COLON
    { try
	prerr_endline (Hashtbl.find rule_tbl $3);
	result := 1
      with Not_found ->
	Hashtbl.add rule_tbl $3 ("Rule \"" ^ $3 ^ "\" already defined")
    }

rule_body:
| rule_side DARROW rule_side {}

rule_side:
| WORD LPAREN RPAREN
    { try
	ignore (Hashtbl.find operator_tbl $1)
      with Not_found ->
	result := 1;
	prerr_endline ("Operator \"" ^ $1 ^ "\" unknown")
    }
| WORD LPAREN rule_side_list RPAREN
    { try
	ignore (Hashtbl.find operator_tbl $1)
      with Not_found ->
	result := 1;
	prerr_endline ("Operator \"" ^ $1 ^ "\" unknown")
    }
| QMARK WORD {}

rule_side_list:
| rule_side COMMA rule_side_list {}
| rule_side {}

%%
