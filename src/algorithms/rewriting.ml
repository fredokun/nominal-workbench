
open Rewriting_ast
open Term_ast
open Strategy_ast
open Symbols

let raise_unknown_placeholder ident =
  let open Rewriting_error in
  raise (RewritingError(UnknownPlaceholder, ident))



let rec substitute placeholders effect =
  match effect with
  | EConstant ident -> Const ident
  | EPlaceholder ident ->
    begin
      try Matching.SMap.find ident placeholders
      with Not_found -> raise_unknown_placeholder ident
    end
  | EOperator (ident, operands) ->
    Term (ident, List.map (substitute placeholders) operands)

 

let rewrite (pattern, effect) ifmatch elsef term =
  match Matching.matching term pattern with None -> elsef term
  | Some ph -> ifmatch ph effect

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
  | SCall(name, s_list) -> 
    SCall(name, List.map (replace ident new_strategy) s_list)

let rec apply_strategy rules rec_env strategy term =
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
          let (_, rule) = System_map.find name rules in
          rewrite rule (fun ph ef -> Some(substitute ph ef)) (fun x -> None) term
        with Not_found -> assert false
      end
    | SRule(None) -> apply_strategy (Strategies.seq_all rules) term
    | SAll(s) -> apply_to_children rules rec_env s term
    | SCall _ -> assert false (* TODO *)
  in
  apply_strategy strategy term

and apply_to_children rules rec_env strategy = function
  | Term(name, terms) ->
    let rec apply_to_children acc = function
      | [] -> Some(Term(name, List.rev acc))
      | head :: tail -> 
        begin
          match apply_strategy rules rec_env strategy head with
          | None -> None
          | Some(t) -> apply_to_children (t::acc) tail
        end
    in
    apply_to_children [] terms
  | t -> Some(t)
    

let rec rewrite_rec strategy rules term =
  let rec_env = Hashtbl.create 3 in
  match apply_strategy rules rec_env strategy term with
  | Some(newterm) when newterm <> term -> rewrite_rec strategy rules newterm
  | Some(newterm) -> newterm
  | None -> assert false

