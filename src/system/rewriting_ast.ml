open Format


type type_name = string
type type_binders = type_name list
type kind_type =
  | Type
  | Atom
type kind = kind_type list
type type_application =
  | TypeApplication of type_name * type_application list
  | TypeName of type_name
type constant = type_binders * type_application
type operator_result = type_application
type operator_arg =
  | OpTypeArg of type_application
  | OpBinderArg of type_name
type operator = type_binders * operator_arg list * operator_result
type pattern =
  | POperator of string * pattern list
  | PPlaceholder of string
  | PConstant of string
  | PAny
type effect =
  | EOperator of string * effect list
  | EPlaceholder of string
  | EConstant of string
type rule = pattern * effect
type info = Lexing.position
type rewriting_decl =
  {
    name : string;
    info : info;
    desc : rewriting_desc;
  }
and rewriting_desc =
  | DKind of kind
  | DConstant of constant
  | DOperator of operator
  | DRule of rule

  
let print_separated sep f fmt = 
  let rec loop = function
  | [] -> ()
  | [e] -> fprintf fmt "%a" f e
  | h::t -> fprintf fmt "%a" f h; loop t
  in loop


let print_kind_type fmt = function
  | Type -> fprintf fmt "type"
  | Atom -> fprintf fmt "atom"

let print_kind = print_separated " -> " print_kind_type


let print_type_name fmt tn = fprintf fmt "%s" tn
  

let rec print_type_application fmt = function
  | TypeApplication (tname, tapps) -> fprintf fmt "%s [%a]" tname
      (print_separated ", " print_type_application) tapps
  | TypeName tname -> print_type_name fmt tname

      
let print_operator_arg fmt = function
  | OpTypeArg tapp -> print_type_application fmt tapp  
  | OpBinderArg tname -> print_type_name fmt tname 

  
let print_type_binders fmt binders =
  print_separated ", " print_type_name fmt binders
  

let print_operator fmt op =
  let binders, args, result = op in
  fprintf fmt "%a %a %a" print_type_binders binders
    (print_separated ", " print_operator_arg) args print_type_application result

  
let rec print_pattern fmt = function
  | POperator (name, patterns) ->
      fprintf fmt "%s (%a)" name (print_separated ", " print_pattern) patterns
  | PPlaceholder s -> fprintf fmt "?%s" s
  | PConstant s -> fprintf fmt "%s" s
  | PAny -> fprintf fmt "_"


let rec print_effect fmt = function
  | EOperator (name, effects) -> 
      fprintf fmt "%s (%a)" name (print_separated ", " print_effect) effects
  | EPlaceholder s -> fprintf fmt "?%s" s
  | EConstant s -> fprintf fmt "%s" s


let print_rule fmt rule =
  let p, e = rule in
  fprintf fmt "%a -> %a" print_pattern p print_effect e

let print_const_decl fmt cst =
  let binders, tapp = cst in
  fprintf fmt "%a %a" print_type_binders binders print_type_application tapp

let print_rewriting_desc name fmt desc =
  match desc with
  | DKind k -> fprintf fmt "Kind %s : %a" name print_kind k
  | DConstant c -> fprintf fmt "Constant %s : %a" name print_const_decl c
  | DOperator o -> fprintf fmt "Operator %s : %a" name print_operator o
  | DRule r -> fprintf fmt "Rule %s : %a" name print_rule r


let print_rewriting_decl fmt rd = fprintf fmt "%a"
  (print_rewriting_desc rd.name) rd.desc




















