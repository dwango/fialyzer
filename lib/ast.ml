open Base
module Format = Caml.Format

module Z = struct
  type t = Z.t
  let sexp_of_t z = Z.to_string z |> sexp_of_string
  let pp fmt z = Z.to_string z |> Caml.Format.fprintf fmt "%s"
end

type line = int
[@@deriving show, sexp_of]

type t =
    | Constant of line * Constant.t
    | Var of line * string
    | Tuple of line * t list
    | App of line * t * t list
    | Abs of line * fun_abst
    | Let of line * string * t * t
    | Letrec of line * (string * fun_abst) list * t
    | Case of t * (pattern * t) list
    | LocalFun of {function_name : string; arity: int}
    | MFA of {module_name: t; function_name: t; arity: t}
    | ListCons of t * t
    | ListNil
and fun_abst = {args: string list; body: t}
and pattern = pattern' * t
and pattern' =
    | PatVar of string
    | PatTuple of pattern' list
    | PatConstant of Constant.t
    | PatCons of pattern' * pattern'
    | PatNil
[@@deriving sexp_of]

let line_number_of_t = function
| Constant (line, _) -> line
| Var (line, _) -> line
| Tuple (line, _) -> line
| App (line, _, _) -> line
| Abs (line, _) -> line
| Let (line, _, _, _) -> line
| Letrec (_) -> -1
| Case (_, _) -> -1
| LocalFun _ -> -1
| MFA _ -> -1
| ListCons (_, _) -> -1
| ListNil -> -1

let string_of_t t =
  [%sexp_of: t] t |> Sexplib.Sexp.to_string_hum ~indent:2

type spec_fun = (Type.t list * Type.t) list
[@@deriving sexp_of]
type decl_fun = {specs: spec_fun option; fun_name: string; fun_abst: fun_abst}
[@@deriving sexp_of]
type module_ = {
    file : string;
    name : string;
    export : (string * int) list;
    functions : decl_fun list;
  }
[@@deriving sexp_of]
