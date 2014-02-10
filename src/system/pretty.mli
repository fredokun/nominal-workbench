val print_separated :
  string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val print_kind_type: Format.formatter -> Rewriting_ast.kind_type -> unit

val print_kind: Format.formatter -> Rewriting_ast.kind -> unit
  
val print_type_name: Format.formatter -> string -> unit

val print_type_application:
  Format.formatter -> Rewriting_ast.type_application -> unit

val print_operator_arg: Format.formatter -> Rewriting_ast.operator_arg -> unit
  
val print_type_binders: Format.formatter -> string list -> unit
  
val print_operator: Format.formatter ->
  string list * Rewriting_ast.operator_arg list *
    Rewriting_ast.type_application -> unit

val print_pattern: Format.formatter -> Rewriting_ast.pattern -> unit

val print_effect: Format.formatter -> Rewriting_ast.effect -> unit

val print_rule: Format.formatter ->
  Rewriting_ast.pattern * Rewriting_ast.effect -> unit


val print_const_decl: Format.formatter ->
           string list * Rewriting_ast.type_application -> unit

val print_rewriting_desc:
  string -> Format.formatter -> Rewriting_ast.rewriting_desc -> unit

val print_rewriting_decl:
  Format.formatter -> Rewriting_ast.rewriting_decl -> unit

val print_system : Format.formatter -> Symbols.system -> unit
