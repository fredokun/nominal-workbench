type strategy = 
  | SId
  | SFail
  | STest of strategy
  | SNot of strategy
  | SAll of strategy
  | SSeq of strategy * strategy
  | SEither of strategy * strategy
  | SRec of string * strategy
  | SVar of string
  | SRule of string option
  | SCall of string * strategy list

type strategy_def = string list * strategy 

val try_app : strategy -> strategy
val repeat : strategy -> strategy
val topdown : strategy -> strategy
val bottomup : strategy -> strategy
val any_rule : strategy

val strategy_def_of_fun : (strategy -> strategy) -> strategy_def

val string_of_strategy : strategy -> string
