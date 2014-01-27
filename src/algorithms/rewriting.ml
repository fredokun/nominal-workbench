
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
    Term (ident, [], List.map (substitute placeholders) operands)


let rewrite rule term =
  let Rule(pattern, effect) = rule in
  let matches, ph = Matching.matching term pattern in
  if matches then substitute ph effect else term


let rec rewrite_rec rules term =
  let rec find_rule = function
    | [] -> None
    | (Rule (pattern, effect)) :: tail ->
      let matches, ph = Matching.matching term pattern in
      if matches then Some (ph, effect) else find_rule tail in
  match find_rule rules with
  | None -> term
  | Some (ph, effect) ->
    rewrite_rec rules (substitute ph effect)
