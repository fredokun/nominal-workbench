
type info = Lexing.position

type 'info expression_raw =
  | Const of 'info ident
  | Abstraction of 'info ident * 'info ident * 'info expression list
  | Call of 'info ident * 'info expression list
  | Var of 'info ident
and 'info expression = ('info expression_raw, 'info) annotated
