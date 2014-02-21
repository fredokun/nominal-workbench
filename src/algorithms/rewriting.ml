
open Rewriting_ast
open Rewriting_error
open Term_ast
open Strategy_ast
open Symbols

let raise_unknown_placeholder ident =
  raise @@ RewritingError(UnknownPlaceholder, ident)

let rec substitute placeholders effect =
  match effect with
  | EConstant ident -> Term_ast.create_term ident Const
  | EPlaceholder ident ->
    begin
      try Matching.SMap.find ident placeholders
      with Not_found -> 
        raise_unknown_placeholder ident
    end
  | EOperator (ident, operands) ->
    Term_ast.create_term ident (Term (List.map (substitute placeholders) operands))


let rewrite (pattern, effect) ifmatch elsef term =
  match Matching.matching term pattern with None -> elsef term
  | Some ph -> ifmatch ph effect

let seq_all rules =
  let try_app s = SEither(s, SId) in
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
  | SChoice(s1, s2) -> 
    SChoice(replace ident new_strategy s1, replace ident new_strategy s2)
(*  | SRec(var, s) ->
    SRec(var, replace ident new_strategy s) *)
  | STest(s) -> STest(replace ident new_strategy s)
  | SNot(s) -> SNot(replace ident new_strategy s)
  | SVar(name) when name = ident -> new_strategy
  | SVar(name) -> SVar(name)
  | SRule(name) -> SRule(name)
  | SAll(s) -> SAll(replace ident new_strategy s)
  | SSome(s) -> SSome(replace ident new_strategy s)
  | SOne(s) -> SOne(replace ident new_strategy s)
  | SProj(i, s) -> SProj(i, replace ident new_strategy s)
  | SCall(name, s_list) -> 
    SCall(name, List.map (replace ident new_strategy) s_list)

let rec apply_strategy system rec_env strategy term_list =
  let apply = apply_strategy system rec_env in
  (* Printf.printf ">> rewriting : %s \n\twith %s\n" 
    (string_of_term_list term) (string_of_strategy strategy); *)
  match strategy with
  | SId -> term_list
  | SFail -> []
  | SSeq(s1, s2) -> 
    begin
      match apply s1 term_list with
      | [] -> []
      | ts -> apply s2 ts
    end
  | SEither(s1, s2) ->
    begin
      match apply s1 term_list with
      | [] -> apply s2 term_list
      | res -> res
    end
(*  | SRec(var, s) as rec_strat ->
    let new_rec_env = (var, rec_strat)::rec_env in
    apply_strategy system new_rec_env s term_list *)
  | SChoice(s1, s2) ->
    (apply s1 term_list) @ (apply s2 term_list)
  | STest(s) ->
    begin
      match apply s term_list with
      | [] -> []
      | _ -> term_list
    end
  | SNot(s) ->
    begin
      match apply s term_list with
      | [] -> term_list
      | _ -> []
    end
  | SVar(name) -> 
    begin
      try
        let rec_strat = List.assoc name rec_env in
        apply rec_strat term_list
      with Not_found -> raise @@ RewritingError(UnboundStrategyVar, name)
    end
  | SRule(Some(name)) ->
    begin
      try
        let (_, rule) = System_map.find name system.rules in
        let rewrite_term t = 
          rewrite rule (fun ph ef -> [substitute ph ef]) (fun x -> []) t 
        in
        List.flatten @@ List.map rewrite_term term_list
      with Not_found -> raise @@ RewritingError(UnknownRule, name)
    end
  | SRule(None) -> apply (seq_all system.rules) term_list
  | SAll(s) -> apply_to_children system rec_env s term_list
  | SSome(s) -> apply_to_some_children system rec_env s term_list
  | SOne(s) -> apply_to_one_child system rec_env s term_list
  | SProj(i, s) -> apply_to_child i system rec_env s term_list 
  | SCall(name, s_list) ->
    begin
      try
        let rec replace_params acc = function
          | [], [] -> acc
          | [], _ -> assert false (* Should not happen : already checked *)
          | _, [] -> assert false
          | ident :: id_tail, s :: s_tail ->
            let new_s = replace ident s acc in
            replace_params new_s (id_tail, s_tail)
        in
        let (_, (signature, body)) = System_map.find name system.strategies in
        let sig_size = List.length signature in
        if sig_size <> (List.length s_list) then
          raise @@ RewritingError(BadStrategyCall, (string_of_int sig_size))
        else
        let new_s = replace_params body (signature, s_list) in
        apply new_s term_list
      with Not_found -> raise @@ RewritingError(UnknownStrategy, name)
    end

and apply_to_children system rec_env strategy term_list = 
  let apply_to_children' = function
    | {name=name; desc=Term(terms); _} ->
      let rec apply_to_children acc = function
        | [] -> [Term_ast.create_term name (Term ( List.rev acc))]
        | head :: tail -> 
          begin
            match apply_strategy system rec_env strategy [head] with
            | [] -> []
            | ts -> 
              List.flatten  @@ 
                List.map (fun t -> apply_to_children (t::acc) tail) ts
          end
      in
      apply_to_children [] terms
    | const_or_var -> [const_or_var]
  in
  List.flatten @@ List.map apply_to_children' term_list

and apply_to_some_children system rec_env strategy term_list = 
  let apply_to_some_children' = function
    | {name=name; desc=Term(terms); _} ->
      let rec apply_to_children acc one_ok = function
        | [] -> 
          if not one_ok then []
          else [Term_ast.create_term name (Term ( List.rev acc))]
        | head :: tail -> 
          begin
            match apply_strategy system rec_env strategy [head] with
            | [] -> apply_to_children (head::acc) one_ok tail
            | ts -> 
              List.flatten @@
                List.map (fun t -> apply_to_children (t::acc) true tail) ts
          end
      in
      apply_to_children [] false terms
    | const_or_var -> []
  in
  List.flatten @@ List.map apply_to_some_children' term_list

and apply_to_one_child system rec_env strategy term_list = 
  let apply_to_one_child' = function
    | {name=name; desc=Term(terms); _} ->
      let rec apply_to_children acc = function
        | [] -> []
        | head :: tail -> 
          begin
            match apply_strategy system rec_env strategy [head] with
            | [] -> apply_to_children (head::acc) tail
            | ts -> 
              let create_new_terms t =
                let new_children = (List.rev acc) @ (t::tail) in
                Term_ast.create_term name (Term new_children)
              in
              List.map create_new_terms ts
          end
      in
      apply_to_children [] terms
    | const_or_var -> []
  in
  List.flatten @@ List.map apply_to_one_child' term_list

and apply_to_child nth system rec_env strategy term_list = 
  let apply_to_child' = function
    | {name=name; desc=Term(terms); _} ->
      let rec apply_to_nth i acc = function
        | [] -> [Term_ast.create_term name (Term ( List.rev acc))]
        | head :: tail when i = nth -> 
          begin
            match apply_strategy system rec_env strategy [head] with
            | [] -> []
            | ts -> 
              List.flatten @@
                List.map (fun t -> apply_to_nth (i + 1) (t::acc) tail) ts
          end
        | head :: tail -> apply_to_nth (i + 1) (head :: acc) tail
      in
      apply_to_nth 0 [] terms 
    | const_or_var -> [const_or_var]
  in
  List.flatten @@ List.map apply_to_child' term_list    

let rec rewrite_rec strategy system term =
(*
  Printf.printf "rewriting : %s \n\twith %s\n" 
    (string_of_term term) (string_of_strategy strategy);
*)
  match apply_strategy system [] strategy term with
  (* | Some(newterm) when newterm <> term -> rewrite_rec strategy system newterm *)
  | [] -> failwith "Strategy application has failed"
  | newterm -> newterm
