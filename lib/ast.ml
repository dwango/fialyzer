open Base

module Z = struct
  type t = Z.t
  let sexp_of_t z = Z.to_string z |> sexp_of_string
  let pp fmt z = Z.to_string z |> Caml.Format.fprintf fmt "%s"
end

type t =
    | Constant of Constant.t
    | Var of string
    | Tuple of t list
    | App of t * t list
    | Abs of string list * t
    | Let of string * t * t
    | Letrec of (string * t) list * t
    | Case of t * (pattern * t) list
    | MFA of {module_name: t; function_name: t; arity: t}
    | ListCons of t * t
    | ListNil
[@@deriving sexp_of]
and pattern = pattern' * t
and pattern' =
    | PatVar of string
    | PatTuple of pattern' list
    | PatConstant of Constant.t
    | PatCons of pattern' * pattern'
    | PatNil
[@@deriving sexp_of]

let string_of_t t =
  [%sexp_of: t] t |> Sexplib.Sexp.to_string_hum ~indent:2

type spec_fun = (Type.t list * Type.t) list
[@@deriving sexp_of]
type decl_fun = {specs: spec_fun option; fun_name: string; args: string list; body: t}
[@@deriving sexp_of]
type module_ = {
    file : string;
    name : string;
    export : (string * int) list;
    functions : decl_fun list;
  }
[@@deriving sexp_of]
