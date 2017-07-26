type constant =
    | Int of int
    | Float of float
    | Atom of string
[@@deriving sexp]

type expr =
    | Val of constant
    | Var of string
    | Struct of expr list
    | App of expr * expr list
    | Abs of string list * expr
    | Let of string * expr * expr
    | Letrec of (string * expr) list * expr
    | Case of expr * (pattern * expr) list
[@@deriving sexp]
and pattern =
    | PatVar of string * expr
    | PatStruct of string list * expr
[@@deriving sexp]

type typ =
    | TyVar of string
    | TyStruct of typ list
    | Fun of typ list * typ
    | Union of typ * typ
    | Constraint of typ * constraint_
    | Any
    | None
    | Integer
    | TyAtom
    | TyConstant of constant
[@@deriving sexp]
and constraint_ =
    | Subtype of typ * typ
    | Conj of constraint_ list
    | Disj of constraint_ list
    | Empty
[@@deriving sexp]
