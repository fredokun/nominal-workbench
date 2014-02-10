
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


let rewrite (pattern, effect) term =
  let matches, ph = Matching.matching term pattern in
  if matches then substitute ph effect else term


let rec rewrite_rec rules term =
  let rec replace = function
    | [], term -> term
    | ((pattern, effect) as rule :: tail) as all_rules, term ->
      let rec step term =
        let matches, ph = Matching.matching term pattern in
        if matches then
          (substitute ph effect)
        else
          let res = match term with
          | Term(name, expr_l) -> Term(name, (List.map step expr_l))
          | other -> other in
          res in
      let newterm = step term in
      replace (tail, newterm) in
  match replace (rules, term) with
  | newterm when newterm <> term -> rewrite_rec rules newterm
  | newterm -> newterm
