open Format
open Symbols
open Rewriting_ast
open Strategy_ast

  
let pp_separated sep f fmt = 
  let rec loop = function
  | [] -> ()
  | [e] -> fprintf fmt "%a" f e
  | h::t -> fprintf fmt "%a%s" f h sep; loop t
  in loop


let pp_kind_type fmt = function
  | Type -> fprintf fmt "type"
  | Atom -> fprintf fmt "atom"

let pp_kind = pp_separated " -> " pp_kind_type


let pp_type_name fmt tn = fprintf fmt "%s" tn
  

let rec pp_type_application fmt = function
  | TypeApplication (tname, tapps) -> fprintf fmt "%s [%a]" tname
      (pp_separated ", " pp_type_application) tapps
  | TypeName tname -> pp_type_name fmt tname

      
let pp_operator_arg fmt = function
  | OpTypeArg tapp -> pp_type_application fmt tapp  
  | OpBinderArg tname -> pp_type_name fmt tname 

  
let pp_type_binders fmt binders =
  match binders with [] -> () | binders ->
    fprintf fmt " forall(%a)." (pp_separated ", " pp_type_name) binders
  

let pp_operator fmt op =
  let binders, args, result = op in
  fprintf fmt "%a%a -> %a" pp_type_binders binders
    (pp_separated " * " pp_operator_arg) args pp_type_application result

  
let rec pp_pattern fmt = function
  | POperator (name, patterns) ->
      fprintf fmt "%s (%a)" name (pp_separated ", " pp_pattern) patterns
  | PPlaceholder s -> fprintf fmt "%s" s
  | PConstant s -> fprintf fmt "%s" s
  | PAny -> fprintf fmt "_"


let rec pp_effect fmt = function
  | EOperator (name, effects) -> 
      fprintf fmt "%s (%a)" name (pp_separated ", " pp_effect) effects
  | EPlaceholder s -> fprintf fmt "%s" s
  | EConstant s -> fprintf fmt "%s" s


let pp_rule fmt rule =
  let p, e = rule in
  fprintf fmt "%a => %a" pp_pattern p pp_effect e

let pp_const_decl fmt cst =
  let binders, tapp = cst in
  fprintf fmt "%a %a" pp_type_binders binders pp_type_application tapp

let rec pp_strategy fmt = function
  | SId -> fprintf fmt "id"
  | SFail -> fprintf fmt "fail"
  | SSeq(s1, s2) -> fprintf fmt "%a; %a" pp_strategy s1 pp_strategy s2
  | SEither(s1, s2) -> fprintf fmt "%a +> %a" pp_strategy s1 pp_strategy s2
  | SRec(var, s) -> fprintf fmt "rec (%s, %a)"  var pp_strategy s
  | STest(s) -> fprintf fmt "test(%a)" pp_strategy s
  | SNot(s) -> fprintf fmt "not(%a)" pp_strategy s
  | SVar(name) -> fprintf fmt "%s" name
  | SRule(Some(name)) -> fprintf fmt "rule(%s)" name
  | SRule(None) -> fprintf fmt "rule()"
  | SAll(s) -> fprintf fmt "all(%a)" pp_strategy s
  | SSome(s) -> fprintf fmt "some(%a)" pp_strategy s
  | SOne(s) -> fprintf fmt "one(%a)" pp_strategy s
  | SProj(i, s) -> fprintf fmt "proj(%d, %a)" i pp_strategy s
  | SCall(name, params) -> fprintf fmt "%s(%a)" name
      (pp_separated ", " pp_strategy) params


let pp_strategy_def fmt (params, s) =
  fprintf fmt " (%a) %a"
    (pp_separated ", " (fun fmt s -> fprintf fmt "%s" s)) params pp_strategy s
    

let pp_rewriting_desc name fmt desc =
  match desc with
  | DKind k -> fprintf fmt "kind %s: %a" name pp_kind k
  | DConstant c -> fprintf fmt "constant %s: %a" name pp_const_decl c
  | DRule r -> fprintf fmt "rule [%s]:%a" name pp_rule r
  | DOperator o -> fprintf fmt "operator %s: %a" name pp_operator o
  | DStrategy s -> fprintf fmt "strategy %s %a" name pp_strategy_def s


let pp_rewriting_decl fmt rd = fprintf fmt "%a"
  (pp_rewriting_desc rd.name) rd.desc


let rec pp_term fmt t = 
  let open Term_ast in
  let {name=name;desc=desc;_} = t in
  match desc with
  | Const -> fprintf fmt "%s" name
  | Var -> fprintf fmt "%s" name
  | Term term_list -> fprintf fmt "%s (%a)" name
      (pp_separated ", " pp_term) term_list


      
let pp_system fmt sys =
  let print f l = System_map.iter
    (fun k (_, d) -> fprintf fmt "%a@." (pp_rewriting_desc k) (f d)) l in
  print (fun x -> DKind x) sys.kinds;
  print (fun x -> DConstant x) sys.constants;
  print (fun x -> DOperator x) sys.operators;
  print (fun x -> DRule x) sys.rules


let string_of print e = asprintf "%a" print e
