type ('a, 'b) annotated = { value : 'a; info : 'b }

type 'info ident = (string, 'info) annotated

type 'a named = string * 'a 

type kind_type = Type | Atom

type kind_raw = kind_type named
type 'info kind = (kind_raw, 'info) annotated

type 'info term_type_raw = 
  | Kind of 'info ident
  | Prod of 'info term_type list
  | Abs of 'info ident
and 'info term_type = ('info term_type_raw, 'info) annotated

type 'info const_raw = 'info term_type named
type 'info const = ('info const_raw, 'info) annotated

type 'info operator_raw  = ('info term_type * 'info term_type) named
type 'info operator = ('info operator_raw, 'info) annotated

type 'info term_pattern_raw =
  | Constant of 'info ident (* True *)
  | Placeholder of 'info ident (* ?x *)
  | Operator of 'info ident * 'info term_pattern list
and 'info term_pattern = ('info term_pattern_raw, 'info) annotated

type 'info rule_raw = ('info term_pattern * 'info term_pattern) named

type 'info rule = ('info rule_raw, 'info) annotated

(* The rewrite system is essentially four environments that depend on each other *)
type 'info definitions = 
  'info kind list * 'info const list * 'info operator list * 'info rule list
