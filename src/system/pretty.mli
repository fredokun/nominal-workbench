val pp_separated :
  string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val pp_kind_type: Format.formatter -> Rewriting_ast.kind_type -> unit

val pp_kind: Format.formatter -> Rewriting_ast.kind -> unit
  
val pp_type_name: Format.formatter -> string -> unit

val pp_type_application:
  Format.formatter -> Rewriting_ast.type_application -> unit

val pp_operator_arg: Format.formatter -> Rewriting_ast.operator_arg -> unit
  
val pp_type_binders: Format.formatter -> string list -> unit
  
val pp_operator: Format.formatter ->
  string list * Rewriting_ast.operator_arg list *
    Rewriting_ast.type_application -> unit

val pp_pattern: Format.formatter -> Rewriting_ast.pattern -> unit

val pp_effect: Format.formatter -> Rewriting_ast.effect -> unit

val pp_rule: Format.formatter ->
  Rewriting_ast.pattern * Rewriting_ast.effect -> unit


val pp_const_decl: Format.formatter ->
           string list * Rewriting_ast.type_application -> unit

val pp_rewriting_desc:
  string -> Format.formatter -> Rewriting_ast.rewriting_desc -> unit

val pp_rewriting_decl:
  Format.formatter -> Rewriting_ast.rewriting_decl -> unit

val pp_strategy : Format.formatter -> Strategy_ast.strategy -> unit
  
val pp_strategy_def :
  Format.formatter -> string list * Strategy_ast.strategy -> unit

val pp_system : Format.formatter -> Symbols.system -> unit

val pp_term : Format.formatter -> Term_ast.term_ast -> unit 
  
val string_of : (Format.formatter -> 'a -> unit) -> 'a -> string
