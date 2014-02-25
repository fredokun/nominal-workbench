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
  | SVar of string
  | SRule of string option
  | SCall of string * strategy list

type strategy_def = string list * strategy 

val try_def : string * string list * strategy
val repeat_def : string * string list * strategy
val topdown_def : string * string list * strategy
val bottomup_def : string * string list * strategy

val base_strat : string -> strategy -> strategy
val base_strat_simple : string -> strategy
