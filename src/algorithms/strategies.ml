open Strategy_ast

let try_app s = SEither(s, SId)
let repeat s = SRec("x", SSeq(try_app s, SVar("x")))
let topdown s = SRec("x", SSeq(s, SAll(SVar("x"))))
let bottomup s = SRec("x", SSeq(SAll(SVar("x")), s))
let any_rule = SRule(None)

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
  

let rec string_of_strategy = function
  | SId -> "id"
  | SFail -> "fail"
  | SSeq(s1, s2) -> 
    (string_of_strategy s1) ^ "; " ^ (string_of_strategy s2)
  | SEither(s1, s2) ->
    (string_of_strategy s1) ^ " +> " ^ (string_of_strategy s2)
  | SRec(var, s) -> "rec(" ^ var ^ ", " ^ (string_of_strategy s) ^ ")"
  | STest(s) -> "test(" ^ (string_of_strategy s) ^ ")"
  | SNot(s) -> "not(" ^ (string_of_strategy s) ^ ")"
  | SVar(name) -> name
  | SRule(Some(name)) -> "rule(" ^ name ^ ")"
  | SRule(None) -> "rule()"
  | SAll(s) -> "all(" ^ (string_of_strategy s) ^ ")"
  | SCall(name, params) -> 
    let params = List.map string_of_strategy params in
    name ^ "(" ^ (String.concat "," params) ^ ")"
