open Strategy

let try_app s = Either(s, Id)
let repeat s = Rec("x", Seq(try_app s, Var("x")))
let topdown s = Rec("x", Seq(s, All(Var("x"))))
let bottomup s = Rec("x", Seq(All(Var("x")), s))

let rec seq_all_rules = function
  | [] -> Id
  | head :: [] -> try_app @@ Rule(head)
  | head :: tail -> Seq(try_app @@ Rule(head), seq_all_rules tail)

let rec string_of_strategy = function
  | Id -> "id"
  | Fail -> "fail"
  | Seq(s1, s2) -> 
    (string_of_strategy s1) ^ "; " ^ (string_of_strategy s2)
  | Either(s1, s2) ->
    (string_of_strategy s1) ^ " +> " ^ (string_of_strategy s2)
  | Rec(var, s) -> var ^ "." ^ (string_of_strategy s)
  | Test(s) -> "test(" ^ (string_of_strategy s) ^ ")"
  | Not(s) -> "not(" ^ (string_of_strategy s) ^ ")"
  | Var(name) -> name
  | Rule(name) -> name
  | All(s) -> "all(" ^ (string_of_strategy s) ^ ")"
