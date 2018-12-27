open Base
module Format = Caml.Format
open Constant
open Type

module Z = struct
  type t = Z.t
  let sexp_of_t z = Z.to_string z |> sexp_of_string
  let pp fmt z = Z.to_string z |> Format.fprintf fmt "%s"
end

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
    | PatConstant of constant
[@@deriving show, sexp_of]

let string_of_expr expr =
  [%sexp_of: expr] expr |> Sexplib.Sexp.to_string_hum ~indent:2

type spec_fun = (typ list * typ) list
[@@deriving show, sexp_of]
type decl_fun = {specs: spec_fun option; fun_name: string; args: string list; body: expr}
[@@deriving show, sexp_of]
type module_ = {
    file : string;
    name : string;
    export : (string * int) list;
    functions : decl_fun list;
  }
[@@deriving show, sexp_of]
