(* Auxiliary types *)
type type_name = string
type type_binders = type_name list

(* Kind definition *)

(* An atom cannot be parametrized by types. *)
(* kind List: type -> type *)
(* kind Variable: atom *)
type kind =
  | Type of type_binders
  | Atom

(* Type application *)
(* A type application is the process to apply a type to another. *)
(* Example: forall(A).List<A> *)
type type_application = type_name * type_name list

(* Constant definition *)

type constant = Constant of type_binders * type_application

(* Operator definition *)

(* A binder is: "Variable" in [Variable].Term * Term -> Term.
 (Note the '.' that delimitates the binder). *)
type binder = type_name option
type operator_args = type_application list
type operator_result = type_name

type operator = Operator of type_binders * binder * operator_args * operator_result

(* Rule definition *)

type pattern = 
  | POperator of string * pattern list
  | PPlaceholder of string
  | PConstant of string
  | PAny

type effect = 
  | EOperator of string * effect list
  | EPlaceholder of string
  | EConstant of string

type rule = Rule of pattern * effect

(* rewriting system *)
type info = Lexing.position
type 'a symbol_table = (string, 'a * info) Hashtbl.t
type rewriting_system = 
  kind symbol_table * 
  constant symbol_table * 
  operator symbol_table *
  rule symbol_table