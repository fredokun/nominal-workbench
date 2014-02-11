

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


