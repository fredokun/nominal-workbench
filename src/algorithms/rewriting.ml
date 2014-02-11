
open Rewriting_ast
open Term_ast
open Strategy
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
  | Id -> Id
  | Fail -> Fail
  | Seq(s1, s2) ->
    Seq(replace ident new_strategy s1, replace ident new_strategy s2)
  | Either(s1, s2) -> 
    Either(replace ident new_strategy s1, replace ident new_strategy s2)
  | Rec(var, s) ->
    Rec(var, replace ident new_strategy s)
  | Test(s) -> Test(replace ident new_strategy s)
  | Not(s) -> Not(replace ident new_strategy s)
  | Var(name) when name = ident -> new_strategy
  | Var(name) -> Var(name)
  | Rule(name) -> Rule(name)
  | All(s) -> All(replace ident new_strategy s)

let rec apply_strategy rules strategy term =
  Printf.printf "applying strat : %s\n" (Strategies.string_of_strategy strategy);
  (* Printf.printf "on term : %s\n" (string_of_term term); *)
  let rec apply_strategy strategy term =
    match strategy with
    | Id -> Some(term)
    | Fail -> None
    | Seq(s1, s2) -> 
      begin
        match apply_strategy s1 term with
        | None -> None
        | Some(t) -> apply_strategy s2 t
      end
    | Either(s1, s2) ->
      begin
        match apply_strategy s1 term with
        | None -> apply_strategy s2 term
        | res -> res
      end
    | Rec(var, s) as rec_strat ->
      apply_strategy (replace var rec_strat s) term
    | Test(s) ->
      begin
        match apply_strategy s term with
        | None -> None
        | Some _ -> Some(term)
      end
    | Not(s) ->
      begin
        match apply_strategy s term with
        | None -> Some(term)
        | Some _ -> None
      end
    | Var _ -> assert false 
    | Rule(name) ->
      begin
        try
          let (_, rule) = System_map.find name rules in
          rewrite rule (fun ph ef -> Some(substitute ph ef)) (fun x -> None) term
        with Not_found -> assert false
      end
    | All(s) -> apply_to_children rules s term
  in
  apply_strategy strategy term

and apply_to_children rules strategy = function
  | Term(name, terms) ->
    let rec apply_to_children acc = function
      | [] -> Some(Term(name, List.rev acc))
      | head :: tail -> 
        begin
          match apply_strategy rules strategy head with
          | None -> None
          | Some(t) -> apply_to_children (t::acc) tail
        end
    in
    apply_to_children [] terms
  | t -> Some(t)
    

let rec bottom_up rule term =
  match term with
  | Term(name, expr_l) -> rewrite rule substitute (fun x -> x)
      (Term(name, (List.map (bottom_up rule) expr_l)))
  | _ -> term 


let rec top_bottom rule term =
  rewrite rule substitute (function
  | Term(name, expr_l) -> Term(name, (List.map (top_bottom rule) expr_l))
  | other -> other
  ) term


let rec rewrite_rec strategy rules term =
  match apply_strategy rules strategy term with
  | Some(newterm) when newterm <> term -> rewrite_rec strategy rules newterm
  | Some(newterm) -> newterm
  | None -> assert false

