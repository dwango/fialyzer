open Base
open Base.Polymorphic_compare
open Common

type t =
  | Eq of {lhs: Type.t; rhs: Type.t; link: Ast.t}
  | Subtype of {lhs: Type.t; rhs: Type.t; link: Ast.t}
  | Conj of t list
  | Disj of t list
  | Empty
[@@deriving sexp_of]

let show c =
  let rec iter indent = function
    | Empty -> ""
    | Eq {lhs=ty1; rhs=ty2; _} ->
       !%"%s%s = %s" indent (Type.pp ty1) (Type.pp ty2)
    | Subtype {lhs=ty1; rhs=ty2; _} ->
       !%"%s%s <: %s" indent (Type.pp ty1) (Type.pp ty2)
    | Conj cs ->
       let ss = List.map ~f:(iter ("  "^indent)) cs |> List.filter ~f:((<>) "") in
       !%"%sConj {\n%s\n%s}" indent (String.concat ~sep:"\n" ss) indent
    | Disj cs ->
       let ss = List.map ~f:(iter ("  "^indent)) cs |> List.filter ~f:((<>) "") in
       !%"%sDisj {\n%s\n%s}" indent (String.concat ~sep:"\n" ss) indent
  in
  iter "" c
