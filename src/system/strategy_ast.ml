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
  | SChoice of strategy * strategy
(*  | SRec of string * strategy *)
  | SVar of string
  | SRule of string option
  | SCall of string * strategy list

type strategy_def = string list * strategy 

let any_rule = SRule(None)

let try_def = 
  let var = "s" in  
  "Try", [var], SEither(SVar(var), SId)

let repeat_def = 
  let name, var = "Repeat", "s" in
  name, [var], SSeq(SVar(var), SCall(name, [SVar(var)]))

let topdown_def = 
  let name, var = "Topdown", "s" in
  name, [var], SSeq(SVar(var), SAll(SCall(name, [SVar(var)])))

let bottomup_def = 
  let name, var = "Bottomup", "s" in
  name, [var], SSeq(SAll(SCall(name, [SVar(var)])), SVar(var))

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
