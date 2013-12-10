%{
  open Hashtbl
  open Parsing
  open Lexing

  let operator_tbl = Hashtbl.create 10
  let constant_tbl = Hashtbl.create 10
  let kind_tbl = Hashtbl.create 10
  let rule_tbl = Hashtbl.create 10
  let result = ref 0

  let err_msg = fun kwd name msg ->
    let pos = Parsing.symbol_start_pos () in
    kwd ^ " \"" ^ name ^"\" " ^ msg ^ " line \
    " ^ (string_of_int (pos.Lexing.pos_lnum)) ^ ", col \
    " ^ (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

  let already_def = "already defined"
  let unk = "unknown"

  let clear_tables = fun x ->
    print_endline "ca clean";
    Hashtbl.clear operator_tbl;
    Hashtbl.clear constant_tbl;
    Hashtbl.clear kind_tbl;
    Hashtbl.clear rule_tbl

%}

/* values */
%token <float> NUM
%token <string> WORD

/* keywords */
%token KIND TYPE ATOM OPERATOR RULE CONSTANT

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET LACCOL RACCOL SEMICOL COLON EQUAL ARROW
%token DARROW STAR COMMA QMARK NEWLINE

%token EOF

%start start
%type <int> start
%type <unit> kind_decl

%start toplevel_phrase
%type <unit> toplevel_phrase

%%

start:
| decls EOF
    { (if !result = 0 then
	print_endline "Success !"
      else
	print_endline "Fail !");
      clear_tables ();
      !result
    }

toplevel_phrase:
| decls SEMICOL SEMICOL { () }
| EOF { raise End_of_file }
;

decls :
| decl decls {}
| decl {}

decl:
| kind_decl {}
| operator_decl {}
| constant_decl {}
| rule_decl {}
| NEWLINE {}


/* kinds */

kind_decl:
| KIND WORD COLON kind_lfth
	{ try
	    prerr_endline (Hashtbl.find kind_tbl $2);
	    result := 1
	  with Not_found ->
	    Hashtbl.add kind_tbl $2 (err_msg "Kind" $2 already_def)
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
	{ begin
	  try
	    prerr_endline (Hashtbl.find constant_tbl $2);
	    result := 1
	  with Not_found ->
	    Hashtbl.add constant_tbl $2 (err_msg "Constant" $2 already_def)
	end;
	  begin
	    try
	      ignore (Hashtbl.find kind_tbl $4);
	      result := 1
	    with Not_found ->
	      prerr_endline (err_msg "Kind" $4 unk)
	  end
	}


      /* operators */

	operator_decl:
| OPERATOR WORD COLON operator_type
	    { try
		prerr_endline (Hashtbl.find operator_tbl $2);
		result := 1
	      with Not_found ->
		Hashtbl.add operator_tbl $2 (err_msg "Operator" $2 already_def)
	    }

	    operator_type:
| WORD
		{ try
		    ignore (Hashtbl.find kind_tbl $1)
		  with Not_found ->
		    result := 1;
		    prerr_endline (err_msg "Kind" $1 unk)
		}
| LBRACKET WORD RBRACKET
		    { try
			ignore (Hashtbl.find kind_tbl $2)
		      with Not_found ->
			result := 1;
			prerr_endline (err_msg "Kind" $2 unk)
		    }
| operator_type STAR operator_type {}
| operator_type ARROW operator_type {}


  /* rules */

    rule_decl:
| rule_head rule_body {}
| rule_head NEWLINE rule_body {}

    rule_head:
| RULE LBRACKET WORD RBRACKET COLON
	{ try
	    prerr_endline (Hashtbl.find rule_tbl $3);
	    result := 1
	  with Not_found ->
	    Hashtbl.add rule_tbl $3 (err_msg "Rule" $3 already_def)
	}

	rule_body:
| rule_side DARROW rule_side {}

	    rule_side:
| WORD LPAREN RPAREN
		{ try
		    ignore (Hashtbl.find operator_tbl $1)
		  with Not_found ->
		    result := 1;
		    prerr_endline (err_msg "Operator" $1 unk)
		}
| WORD LPAREN rule_side_list RPAREN
		    { try
			ignore (Hashtbl.find operator_tbl $1)
		      with Not_found ->
			result := 1;
			prerr_endline (err_msg "Operator" $1 unk)
		    }
| QMARK WORD {}

			rule_side_list:
| rule_side COMMA rule_side_list {}
| rule_side {}


%%
