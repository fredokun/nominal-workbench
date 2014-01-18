(* Auxiliary types *)
type type_name = string
type type_binders = type_name list

(* Kind definition *)

(* kind List: type -> type *)
(* kind Variable: atom *)
type kind_type =
  | Type
  | Atom

type kind = Kind of kind_type list

(* Type application *)
(* A type application is the process to apply a type to another. *)
(* Example: forall(A).List<Pair<A,B> > *)
type type_application =
  | TypeApplication of type_name * type_application list
  | TypeName of type_name

(* Constant definition *)

type constant = Constant of type_binders * type_application

(* Operator definition *)

(* A binder is: "Variable" in [Variable].Term * Term -> Term.
 (Note the '.' that delimitates the binder). *)
type operator_result = type_application (* because you can have something like list<A> -> List<A> *)
type operator_arg =
  | OpTypeArg of type_application
  | OpBinderArg of type_name (* A binder must be of kind atom and thus, cannot be applied to types (List<A>). *)

type operator = Operator of type_binders * operator_arg list * operator_result

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

type rewriting_declaration =
  | DKind of kind
  | DConstant of constant
  | DOperator of operator
  | DRule of rule

type info = Lexing.position
type rewriting_ast = RewritingAST of (string * info * rewriting_declaration) list
