open Format
open Rewriting_ast
open Symbols

  
let print_separated sep f fmt = 
  let rec loop = function
  | [] -> ()
  | [e] -> fprintf fmt "%a" f e
  | h::t -> fprintf fmt "%a%s" f h sep; loop t
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
  match binders with [] -> () | binders ->
    fprintf fmt " forall(%a)." (print_separated ", " print_type_name) binders
  

let print_operator fmt op =
  let binders, args, result = op in
  fprintf fmt "%a%a -> %a" print_type_binders binders
    (print_separated " * " print_operator_arg) args print_type_application result

  
let rec print_pattern fmt = function
  | POperator (name, patterns) ->
      fprintf fmt "%s (%a)" name (print_separated ", " print_pattern) patterns
  | PPlaceholder s -> fprintf fmt "%s" s
  | PConstant s -> fprintf fmt "%s" s
  | PAny -> fprintf fmt "_"


let rec print_effect fmt = function
  | EOperator (name, effects) -> 
      fprintf fmt "%s (%a)" name (print_separated ", " print_effect) effects
  | EPlaceholder s -> fprintf fmt "%s" s
  | EConstant s -> fprintf fmt "%s" s


let print_rule fmt rule =
  let p, e = rule in
  fprintf fmt "%a => %a" print_pattern p print_effect e

let print_const_decl fmt cst =
  let binders, tapp = cst in
  fprintf fmt "%a %a" print_type_binders binders print_type_application tapp

let print_rewriting_desc name fmt desc =
  match desc with
  | DKind k -> fprintf fmt "kind %s : %a" name print_kind k
  | DConstant c -> fprintf fmt "constant %s :%a" name print_const_decl c
  | DOperator o -> fprintf fmt "operator %s :%a" name print_operator o
  | DRule r -> fprintf fmt "rule [%s] : %a" name print_rule r


let print_rewriting_decl fmt rd = fprintf fmt "%a"
  (print_rewriting_desc rd.name) rd.desc


let print_system fmt sys =
  let print f l = System_map.iter
    (fun k (_, d) -> fprintf fmt "%a@." (print_rewriting_desc k) (f d)) l in
  print (fun x -> DKind x) sys.kinds;
  print (fun x -> DConstant x) sys.constants;
  print (fun x -> DOperator x) sys.operators;
  print (fun x -> DRule x) sys.rules;
