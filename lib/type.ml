open Base

type t =
    | TyVar of Type_variable.t
    | TyTuple of t list
    | TyFun of t list * t
    | TyUnion of t * t
    | TyAny
    | TyBottom
    | TyNumber
    | TyAtom
    | TySingleton of Constant.t
[@@deriving sexp_of]

type constraint_ =
    | Eq of t * t
    | Subtype of t * t
    | Conj of constraint_ list
    | Disj of constraint_ list
    | Empty
[@@deriving sexp_of]

(* ref: http://erlang.org/doc/reference_manual/typespec.html *)
let rec pp = function
  | TyVar var -> Type_variable.to_string var
  | TyTuple ts -> "{" ^ (ts |> List.map ~f:pp |> String.concat ~sep:", ") ^ "}"
  | TyFun (args, ret) ->
     let args_str = "(" ^ (args |> List.map ~f:pp |> String.concat ~sep:", ") ^ ")" in
     let ret_str = pp ret in
     "fun(" ^ args_str ^ " -> " ^ ret_str ^ ")"
  | TyUnion (tyl, tyr) -> pp tyl ^ " | " ^ pp tyr
  | TyAny -> "any()"
  | TyBottom -> "none()"
  | TyNumber -> "number()"
  | TyAtom -> "atom()"
  | TySingleton c -> Constant.pp c
