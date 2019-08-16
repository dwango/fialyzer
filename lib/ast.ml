open Base
module Format = Caml.Format
open Common

module Z = struct
  type t = Z.t
  let sexp_of_t z = Z.to_string z |> sexp_of_string
  let pp fmt z = Z.to_string z |> Caml.Format.fprintf fmt "%s"
end

type line = int
[@@deriving show, sexp_of]

type t =
    | Constant of line * Constant.t
    | Ref of line * reference
    | Tuple of line * t list
    | App of line * t * t list
    | Abs of line * fun_abst
    | Let of line * string * t * t
    | Letrec of line * (reference * fun_abst) list * t
    | Case of line * t * (pattern * t) list
    | ListCons of line * t * t
    | ListNil of line
    | MapCreation of line * (t * t) list                  (* #{k1 => v1, ...} *)
    | MapUpdate of {line: int; map: t; assocs: (t * t) list; exact_assocs: (t * t) list} (* M#{k1 => v1, ..., ke1 := ve1, ...} *)
    | Catch of line * t
and fun_abst = {args: string list; body: t}
and pattern = pattern' * t
and pattern' =
    | PatVar of line * string
    | PatTuple of line * pattern' list
    | PatConstant of line * Constant.t
    | PatCons of line * (pattern' * pattern')
    | PatNil of line
    | PatMap of line * (pattern' * pattern') list
and reference =
  | Var of string
  | LocalFun of {function_name : string; arity : int}
  | MFA of {module_name : t; function_name : t; arity : t}
[@@deriving sexp_of]

let universal_pattern line = PatVar (line, "_")

let line_number_of_t = function
| Constant (line, _) -> line
| Ref (line, _) -> line
| Tuple (line, _) -> line
| App (line, _, _) -> line
| Abs (line, _) -> line
| Let (line, _, _, _) -> line
| Letrec (line, _, _) -> line
| Case (line, _, _) -> line
| ListCons (line, _, _) -> line
| ListNil line -> line
| MapCreation (line, _) -> line
| MapUpdate {line;_} -> line
| Catch (line, _) -> line

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

let specs_of_module mod_ : (Mfa.t * Type.t) list =
  let module_name = mod_.name in
  let specs_of_decl fun_decl =
    let function_name = fun_decl.fun_name in
    let mfa arity = Mfa.{module_name; function_name; arity} in
    fun_decl.specs
    |> list_of_option
    |> List.concat
    |> list_group_by ~f:(fun (args, range) -> List.length args)
    |> List.map ~f:(fun (arity, specs) -> (mfa arity, specs))
    |> List.Assoc.map ~f:(List.map ~f:(fun (args, range) -> Type.(of_elem (TyFun (args, range)))))
    |> List.Assoc.map ~f:Type.union_list
  in
  mod_.functions
  |> List.concat_map ~f:specs_of_decl
