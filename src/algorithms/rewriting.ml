
open Rewriting_ast
open Term_ast
open Strategy_ast
open Symbols

let raise_unknown_placeholder ident =
  let open Rewriting_error in
  raise (RewritingError(UnknownPlaceholder, ident))

let rec substitute placeholders effect =
  match effect with
  | EConstant ident -> Term_ast.create_term ident Const
  | EPlaceholder ident ->
    begin
      try Matching.SMap.find ident placeholders
      with Not_found -> raise_unknown_placeholder ident
    end
  | EOperator (ident, operands) ->
    Term_ast.create_term ident (Term (List.map (substitute placeholders) operands))


let rewrite (pattern, effect) ifmatch elsef term =
  match Matching.matching term pattern with None -> elsef term
  | Some ph -> ifmatch ph effect

let seq_all rules =
  let rec seq_all_rules = function
  | [] -> SId
  | head :: [] -> try_app @@ SRule(Some(head))
  | head :: tail -> SSeq(try_app @@ SRule(Some(head)), seq_all_rules tail)
  in
  let rule_names = List.map (fun (n, (_, _)) -> n)
    (Symbols.System_map.bindings rules)
  in
  seq_all_rules rule_names

let rec replace ident new_strategy = function
  | SId -> SId
  | SFail -> SFail
  | SSeq(s1, s2) ->
    SSeq(replace ident new_strategy s1, replace ident new_strategy s2)
  | SEither(s1, s2) -> 
    SEither(replace ident new_strategy s1, replace ident new_strategy s2)
  | SRec(var, s) ->
    SRec(var, replace ident new_strategy s)
  | STest(s) -> STest(replace ident new_strategy s)
  | SNot(s) -> SNot(replace ident new_strategy s)
  | SVar(name) when name = ident -> new_strategy
  | SVar(name) -> SVar(name)
  | SRule(name) -> SRule(name)
  | SAll(s) -> SAll(replace ident new_strategy s)
  | SProj(i, s) -> SProj(i, replace ident new_strategy s)
  | SCall(name, s_list) -> 
    SCall(name, List.map (replace ident new_strategy) s_list)

let rec apply_strategy system rec_env strategy term =
  let rec apply_strategy strategy term =
    match strategy with
    | SId -> Some(term)
    | SFail -> None
    | SSeq(s1, s2) -> 
      begin
        match apply_strategy s1 term with
        | None -> None
        | Some(t) -> apply_strategy s2 t
      end
    | SEither(s1, s2) ->
      begin
        match apply_strategy s1 term with
        | None -> apply_strategy s2 term
        | res -> res
      end
    | SRec(var, s) as rec_strat ->
      Hashtbl.add rec_env var rec_strat;
      apply_strategy s term
    | STest(s) ->
      begin
        match apply_strategy s term with
        | None -> None
        | Some _ -> Some(term)
      end
    | SNot(s) ->
      begin
        match apply_strategy s term with
        | None -> Some(term)
        | Some _ -> None
      end
    | SVar(name) -> 
      begin
        try
          let rec_strat = Hashtbl.find rec_env name in
          apply_strategy rec_strat term
        with Not_found -> assert false
      end
    | SRule(Some(name)) ->
      begin
        try
          let (_, rule) = System_map.find name system.rules in
          rewrite rule (fun ph ef -> Some(substitute ph ef)) (fun x -> None) term
        with Not_found -> assert false
      end
    | SRule(None) -> apply_strategy (seq_all system.rules) term
    | SAll(s) -> apply_to_children system rec_env s term
    | SProj(i, s) -> apply_to_child i system rec_env s term 
    | SCall(name, s_list) ->
      begin
        try
          let rec replace_params acc = function
            | [], [] -> acc
            | [], _ -> assert false
            | _, [] -> assert false
            | ident :: id_tail, s :: s_tail ->
              let new_s = replace ident s acc in
              replace_params new_s (id_tail, s_tail)
          in
          let (_, (signature, body)) = System_map.find name system.strategies in
          let new_s = replace_params body (signature, s_list) in
          apply_strategy new_s term
        with Not_found -> assert false
      end
          
  in
  apply_strategy strategy term

and apply_to_children system rec_env strategy = function
  | {name=name; desc=Term(terms); _} ->
    let rec apply_to_children acc = function
      | [] -> Some(Term_ast.create_term name (Term ( List.rev acc)))
      | head :: tail -> 
        begin
          match apply_strategy system rec_env strategy head with
          | None -> None
          | Some(t) -> apply_to_children (t::acc) tail
        end
    in
    apply_to_children [] terms
  | t -> Some(t)

and apply_to_child nth system rec_env strategy = function
  | {name=name; desc=Term(terms); _} ->
    let rec apply_to_nth i acc = function
      | [] -> Some(Term_ast.create_term name (Term ( List.rev acc)))
      | head :: tail when i = nth -> 
        begin
          match apply_strategy system rec_env strategy head with
          | None -> None
          | Some(t) -> apply_to_nth (i + 1) (t::acc) tail
        end
      | head :: tail -> apply_to_nth (i + 1) (head :: acc) tail
    in
    apply_to_nth 0 [] terms 
  | t -> Some(t)
    

let rec rewrite_rec strategy system term =
  let rec_env = Hashtbl.create 3 in
  match apply_strategy system rec_env strategy term with
  | Some(newterm) when newterm <> term -> rewrite_rec strategy system newterm
  | Some(newterm) -> newterm
  | None -> assert false

