

type strategy = 
  | Id
  | Fail
  | Seq of strategy * strategy
  | Either of strategy * strategy
  | Rec of string * strategy
  | Test of strategy
  | Not of strategy
  | Var of string
  | Rule of string

type outcome = Sucess | Fail

