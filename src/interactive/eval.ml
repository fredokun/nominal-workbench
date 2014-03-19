(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright NoWork Team *)

open Parsing_ast
open Term_ast
open Printf
open Display_test

(* TODO : mli !! *)

let directive_usage =
  String.concat "\n\t"
    [ "Available commands :"
    ; ":help            -- Displays this help"
    ; ":?               -- Displays this help"
    ; ":load-test f exp -- Load a test file f and check that the result matches the expectation"
    ; ":test            -- Test an expression (consult user-doc for more infos)"
    ; ":match           -- Check that an expression matches the given type or strategy"
    ; ":type            -- Returns the type of the expression"
    ; ":dot t out       -- Output the hash-consed version of a term into a dot file graph"
    ; ":quit            -- Exits the REPL"
    ; ":exit            -- Exits the REPL"
    ; ":q               -- Exits the REPL"
    ]
  ^ "\nMore detailled informations may be found in the user-documentation"

let find_file fname =
  let is_file_in_dir dir =
    Sys.file_exists (dir ^ "/" ^ fname)
  in
  try
    List.find is_file_in_dir (Config.get_path ())
    ^ "/" ^ fname
  with Not_found ->
    Printf.eprintf "[Error] Cannot find the file %s\n%!" fname;
    exit 1

let system_name filename = (Filename.chop_extension (Filename.basename filename))

(* Only a textual equality test. *)
let equal_term t1 t2 =
  (strip_ws t1) = (strip_ws t2)

let equal_terms t1s t2s =
  let t1s_str = string_of_terms t1s in
  let t2s_str = string_of_terms t2s in
  try
    List.for_all2 equal_term t1s_str t2s_str
  with
  | Invalid_argument _ -> false

let rec process_file env fname =
  let fpath =
    if Filename.is_implicit fname then
      find_file fname
    else if Sys.file_exists fname then
      fname
    else
    begin
      Printf.eprintf "[Error] Cannot find the file %s\n%!" fname;
      exit 1
    end
  in
  begin
    let ic = open_in fpath in
    try
      let structure = Parser.start Lexer.token (Lexing.from_channel ic) in
      let new_system =
        List.fold_left evaluate_structure_item env structure in
      close_in ic;
      new_system
    with
    | _ ->
      Printf.eprintf "[Warning] Unhandled error. Skipping %s\n%!" fname;
      close_in ic;
      env
  end

and run_term_type_check env term =
  let open Term_checker in
  let ast_checked = construct_ast_checked env term in
  ignore (check_type_of_term env ast_checked)

and process_term env strategy ts : Term_ast.term_ast list =
  let nts = Rewriting.rewrite_rec strategy env ts in
  if not !Config.no_warning then begin
    Printf.printf "Terms : %s rewrote into :%s\n%!"
      (String.concat "; " @@ List.map Pretty.(string_of pp_term) ts)
      (String.concat "; " @@ List.map Pretty.(string_of pp_term) nts)
  end;
  nts

and expanse_term env : term_ast -> term_ast = 
    function {name=n; desc=d; info=info} as id ->
      match d with
	| Term l -> {id with desc = Term (List.map (expanse_term env) l)}
	(* We only allows lower ident globals *)
	| Var when n.[0] >= 'a' && n.[0] <= 'z' -> begin
	  try
	    Symbols.System_map.find n env.Symbols.globals
	  with
	    | Not_found -> id
	end
	| _ -> id
	  
and process_term_expr env : Parsing_ast.term_expr -> Term_ast.term_ast list  = function
  | PTermLet (ident, term_expr) ->
    failwith "Cannot declare global variables in declarations"
  | PTermRewrite (term_expr, strategy) ->
    let rewritten_subterm = process_term_expr env term_expr in
    let rewritten_terms = process_term env strategy rewritten_subterm in
    rewritten_terms
  | PTerm (term) ->
    let term = expanse_term env term in
    run_term_type_check env term; 
    [term]

and evaluate_structure_item env =
  let open Rewriting_ast in
  let open Strategy_ast in
  function
  | PInteractiveCmd cmd -> eval_interactive_cmd env cmd
  | PDecl rewriting_decl ->
    Symbols.enter_decl env rewriting_decl
  | PTermExpr (PTermLet (ident, expr)) ->
    let open Symbols in
    let rewritten_term = process_term_expr env expr in
    {env with globals = System_map.add ident (List.hd rewritten_term) env.globals}
  | PTermExpr term -> ignore (process_term_expr env term); env
  | PFile_include fname -> process_file env fname

and run_type_check env ast =
  List.iter (function
    | PDecl d ->
      Type_checking.check_decl env d
    | _ -> ())
    ast

and eval_and_check env ast =
  let env = List.fold_left evaluate_structure_item env ast in
  run_type_check env ast

and eval_interactive_cmd env = function
| LoadTest(filename, expectation) ->
  load_test_cmd env (RewritingTest(filename, expectation));
  env
| TermTest(term) -> term_test_cmd env term; env
| TermMatch(term, pattern) ->
  match_term_cmd env term pattern;
  env
| TermType(term_expr) ->
  term_type_cmd env term_expr;
  env
| TermMatchType (term_expr, type_binders, arg_types) ->
  term_match_type_cmd env term_expr type_binders arg_types;
  env
| TermToDot (term_expr, filename) ->
  term_to_dot_cmd env term_expr filename;
  env
| Quit -> exit 0
| Help -> print_endline directive_usage; env

(* Interactive commands *)

and load_test_cmd env (RewritingTest(filename, _) as test) =
  let launch () =
    try
      if Sys.is_directory filename then
        print_system_error (sprintf "%s: is a directory" filename)
      else
        let f = open_in filename in
        test_rewriting_system env f test;
        close_in f
    with
    | Sys_error(e) -> print_system_error e
    | e -> print_unknown_exc e (sprintf "launching of the test (%s)" filename) in
  Printexc.record_backtrace true;
  launch ();
  Printexc.record_backtrace false

and test_rewriting_system env channel (RewritingTest(filename, expectation) as test) =
  let open Rewriting_parsing_error in
  try
    check_rewriting_system env (Util.parse_channel channel) test
  with
  | RewritingParsingError(code, msg) ->
      check_expectation filename expectation
        (Failed(Error(domain_name, string_of_error_code code), error_msg code msg)) domain_name
  | e -> print_unknown_exc e (sprintf "parsing of the rewriting system (%s)" filename)

and check_rewriting_system env ast (RewritingTest(filename, expectation)) =
  let open Rewriting_system_error in
  try
    ignore (eval_and_check env ast);
    check_expectation filename expectation Passed domain_name
  with
  | RewritingSystemError(code, msg) ->
    check_expectation filename expectation
      (Failed(Error(domain_name, string_of_error_code code), error_msg code msg)) domain_name
  | e -> print_unknown_exc e (sprintf "check of the rewriting system (%s)" filename)

and check_expectation filename expectation result domain =
  match expectation, result with
  | MustPass, Failed(e, msg) ->
    print_failure (
      sprintf "Failure with error %s.\n" (string_of_error e) ^
      string_of_test_info filename msg)
  | MustFail(e), Passed ->
    print_failure (
      sprintf "Should have failed with %s.\n" (string_of_error e) ^
      string_of_filename filename)
  | MustFail(expected), Failed(e, msg) when not ( equal_error expected e ) ->
      print_failure (sprintf "Expected error %s but failed with %s.\n"
        (string_of_error expected)
        (string_of_error e) ^
        string_of_test_info filename msg)
  | MustFail(_), Failed(e, msg) ->
      print_success (
        sprintf "Failure with %s as expected.\n" (string_of_error e) ^
        string_of_test_info filename msg)
  | MustPass, Passed ->
      print_success (
        sprintf "%s passed.\n" (system_name filename) ^
        string_of_filename filename)

and match_term_cmd env term_expr pattern =
  try
    ignore (Type_checking.check_pattern env pattern);
    List.iter (fun term ->
        let res =
          match Matching.matching
                  (Term_checker.construct_ast_checked env term)
                  pattern with
          | Some _ -> "The term matches the pattern."
          | None -> "The term doesn't match the pattern." in
        print_endline res) @@ process_term_expr env term_expr
  with Rewriting_system_error.RewritingSystemError _ ->
    print_system_error "Pattern ill-formed\n"

and term_type_cmd env term_expr =
  let open Term_checker in
  List.iter (fun term -> print_endline
    @@ Pretty.(string_of pp_type_application)
    @@ type_of_typed_term
    @@ check_type_of_term env
    @@ construct_ast_checked env term)
    (process_term_expr env term_expr)

and term_match_type_cmd env term_expr type_binders arg_types =
  if List.length arg_types <> 1 then
    print_failure "Give one type please"
  else
    let arg_type = List.hd arg_types in
    let open Term_checker in
    let open Term_system_error in
    let gen_binders = binders_to_TBinds type_binders in
    let check_term_type term =
      let term_checked = construct_ast_checked env term in
      try
        begin
          ignore (unify_term_and_type env gen_binders term_checked arg_type);
          print_success (sprintf "The term %s match the type %s\n"
                           (Pretty.(string_of pp_term term))
                           (Pretty.(string_of pp_operator_arg arg_type)))
        end
      with
      | TermSystemError (code, info) ->
        print_failure (sprintf "The term %s does not match the type %s with error :\n%s\n"
                         (Pretty.(string_of pp_term term))
                         (Pretty.(string_of pp_operator_arg arg_type))
                         (error_msg code info))
      | e -> print_unknown_exc e (sprintf "type checking of the term %s\n"
                                    (Pretty.(string_of pp_term term))) in
    List.iter check_term_type (process_term_expr env term_expr)


and term_to_dot_cmd env term_expr filename =
  let open Term_ast_hashconsed in
  if not (Filename.check_suffix filename ".dot") then
    printf "The file should have a .dot extension."
  else
    let filename = try Filename.chop_extension filename
      with _ -> filename in
    let ext = ".dot" in
    ignore @@ List.fold_left (fun n term ->
        let term = Term_checker.construct_ast_checked env term in
        dot (create_term term) (sprintf "%s%d%s" filename n ext);
        n+1) 0 @@ process_term_expr env term_expr

and term_test_cmd env = function
  | TMustPass (InPredicate(t1, t2))
  | TMustPass (EqualPredicate(t1, t2)) ->
  begin
    try
      let rt1s = process_term_expr env t1 in
      let rt2s = List.flatten @@ List.map (process_term_expr env) t2 in
      if (Terms_predicate.term_equality env rt1s rt2s) then
        rewritten_success rt1s rt2s
      else
        rewritten_failure_unexpected rt1s rt2s
    with
    | Rewriting_error.RewritingError(code, msg) -> Rewriting_error.(
        rewritten_failure (string_of_error_code code)
          (string_of_msg (error_msg code msg)))
    | Term_system_error.TermSystemError(code, msg) -> Term_system_error.(
        rewritten_failure (string_of_error_code code)
            (string_of_msg (error_msg code msg)))
    | e -> print_unknown_exc e "term rewriting"
  end
  | TMustFail (term, e) ->
    try
      let rt = flatten_string_of_terms @@ process_term_expr env term in
      print_failure (
        sprintf "Should have failed with %s but passed with %s.\n"
          (string_of_error e) rt)
    with
    | Rewriting_error.RewritingError(code, msg) -> Rewriting_error.(
      if equal_error e (Error(domain_name, string_of_error_code code)) then
        print_success (
          sprintf "Failure with %s as expected.\n" (string_of_error e) ^
          string_of_msg @@ error_msg code msg)
      else
        print_failure (
          sprintf "Expected error %s but failed with %s.\n"
            (string_of_error e)
            (string_of_error (Error(domain_name, string_of_error_code code))) ^
          string_of_msg @@ error_msg code msg))
    | Term_system_error.TermSystemError(code, msg) -> Term_system_error.(
      if equal_error e (Error(domain_name, string_of_error_code code)) then
        print_success (
          sprintf "Failure with %s as expected.\n" (string_of_error e) ^
          string_of_msg @@ error_msg code msg)
      else
        print_failure (
          sprintf "Expected error %s but failed with %s.\n"
            (string_of_error e)
            (string_of_error (Error(domain_name, string_of_error_code code))) ^
          string_of_msg @@ error_msg code msg))
    | e -> print_unknown_exc e "term rewriting"

