type strategy = 
  | SId
  | SFail
  | STest of strategy
  | SNot of strategy
  | SAll of strategy
  | SSome of strategy
  | SOne of strategy
  | SProj of int * strategy
  | SSeq of strategy * strategy
  | SEither of strategy * strategy
  | SRec of string * strategy
  | SVar of string
  | SRule of string option
  | SCall of string * strategy list

type strategy_def = string list * strategy 

let try_app s = SEither(s, SId)
let repeat s = SRec("x", SSeq(try_app s, SVar("x")))
let topdown s = SRec("x", SSeq(s, SAll(SVar("x"))))
let bottomup s = SRec("x", SSeq(SAll(SVar("x")), s))
let any_rule = SRule(None)

let strategy_def_of_fun f = 
  let var = "_s" in
  ([var], f @@ SVar(var))


let base_strat name content =
  match name with
  | "test" -> STest content
  | "not" -> SNot content
  | "all" -> SAll content
  | "some" -> SSome content
  | "one" -> SOne content
  | _ -> assert false

let base_strat_simple = function
  | "id" -> SId
  | "fail" -> SFail
  | _ -> assert false
