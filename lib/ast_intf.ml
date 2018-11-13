let sexp_of_int = Base.sexp_of_int
let sexp_of_float = Base.sexp_of_float
let sexp_of_string = Base.sexp_of_string
let sexp_of_list = Base.sexp_of_list
let sexp_of_option = Base.sexp_of_option

type constant =
    | Int of int
    | Float of float
    | Atom of string
    | String of string
[@@deriving show, sexp_of]

type expr =
    | Val of constant
    | Var of string
    | Struct of expr list
    | App of expr * expr list
    | Abs of string list * expr
    | Let of string * expr * expr
    | Letrec of (string * expr) list * expr
    | Case of expr * (pattern * expr) list
[@@deriving show, sexp_of]
and pattern = pattern_ * expr
and pattern_ =
    | PatVar of string
    | PatStruct of pattern_ list
[@@deriving show, sexp_of]

let string_of_expr expr =
  [%sexp_of: expr] expr |> Sexplib.Sexp.to_string_hum ~indent:2

type typ =
    | TyVar of Type_variable.t
    | TyStruct of typ list
    | TyFun of typ list * typ
    | TyUnion of typ * typ
    | TyConstraint of typ * constraint_
    | TyAny
    | TyNone
    | TyInteger
    | TyAtom
    | TyConstant of constant
[@@deriving show, sexp_of]
and constraint_ =
    | Eq of typ * typ
    | Subtype of typ * typ
    | Conj of constraint_ list
    | Disj of constraint_ list
    | Empty
[@@deriving show, sexp_of]

let string_of_typ typ =
  [%sexp_of: typ] typ |> Sexplib.Sexp.to_string_hum ~indent:2
let string_of_constraint c =
  [%sexp_of: constraint_] c |> Sexplib.Sexp.to_string_hum ~indent:2

type spec_fun = typ list * typ
[@@deriving show, sexp_of]
type decl_fun = spec_fun option * string * string list * expr
[@@deriving show, sexp_of]
type module_ = {
    file : string;
    name : string;
    export : (string * int) list;
    functions : decl_fun list;
  }
[@@deriving show, sexp_of]
