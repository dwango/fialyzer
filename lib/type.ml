open Base
module Format = Caml.Format
open Constant
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
