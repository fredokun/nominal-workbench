
open Rewriting_ast
open Term_ast

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
  let rec replace = function
  | [], term -> term
  | (pattern, effect) as rule :: tail, term ->
      let newterm = strategy rule term in
      replace (tail, newterm)
  in
  match replace (rules, term) with
  | newterm when newterm <> term -> rewrite_rec strategy rules newterm
  | newterm -> newterm
