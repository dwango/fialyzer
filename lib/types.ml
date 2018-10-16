type constant =
    | Int of int
    | Float of float
    | Atom of string
    | String of string
[@@deriving show]

type expr =
    | Val of constant
    | Var of string
    | Struct of expr list
    | App of expr * expr list
    | Abs of string list * expr
    | Let of string * expr * expr
    | Letrec of (string * expr) list * expr
    | Case of expr * (pattern * expr) list
[@@deriving show]
and pattern =
    | PatVar of string * expr
    | PatStruct of string list * expr
[@@deriving show]

type typ =
    | TyVar of string
    | TyStruct of typ list
    | TyFun of typ list * typ
    | TyUnion of typ * typ
    | TyConstraint of typ * constraint_
    | TyAny
    | TyNone
    | TyInteger
    | TyAtom
    | TyConstant of constant
[@@deriving show]
and constraint_ =
    | Eq of typ * typ
    | Subtype of typ * typ
    | Conj of constraint_ list
    | Disj of constraint_ list
    | Empty
[@@deriving show]

type spec_fun = typ list * typ
[@@deriving show]
type decl_fun = spec_fun option * string * string list * expr
[@@deriving show]
type module_ = {
    file : string;
    name : string;
    export : (string * int) list;
    functions : decl_fun list;
  }
[@@deriving show]
