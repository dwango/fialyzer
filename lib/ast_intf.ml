let sexp_of_int = Base.sexp_of_int
let sexp_of_float = Base.sexp_of_float
let sexp_of_string = Base.sexp_of_string
let sexp_of_list = Base.sexp_of_list
let sexp_of_option = Base.sexp_of_option

module Z = struct
  type t = Z.t
  let sexp_of_t z = Z.to_string z |> sexp_of_string
  let pp fmt z = Z.to_string z |> Format.fprintf fmt "%s"
end

type constant =
    | Number of int
    | Atom of string
[@@deriving show, sexp_of]

type expr =
    | Constant of constant
    | Var of string
    | Tuple of expr list
    | App of expr * expr list
    | Abs of string list * expr
    | Let of string * expr * expr
    | Letrec of (string * expr) list * expr
    | Case of expr * (pattern * expr) list
    | MFA of {module_name: expr; function_name: expr; arity: expr}
[@@deriving show, sexp_of]
and pattern = pattern' * expr
and pattern' =
    | PatVar of string
    | PatTuple of pattern' list
    | PatNil
    | PatCons of pattern' * pattern'
[@@deriving show, sexp_of]

let string_of_expr expr =
  [%sexp_of: expr] expr |> Sexplib.Sexp.to_string_hum ~indent:2

type typ =
    | TyVar of Type_variable.t
    | TyTuple of typ list
    | TyFun of typ list * typ
    | TyUnion of typ * typ
    | TyAny
    | TyBottom
    | TyNumber
    | TyAtom
    | TySingleton of constant
[@@deriving show, sexp_of]

type constraint_ =
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
