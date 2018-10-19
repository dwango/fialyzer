open Base
open Ast_intf

type ty_var = string
type sol = (ty_var, typ, String.comparator_witness) Map.t

(* 型 type_subst (X, τ_1) τ_2 := [X ↦ τ_1]τ_2 *)
let rec type_subst ((x : ty_var) , (ty1 : typ)): typ -> typ = function
  | TyStruct tys ->
     TyStruct(List.map ~f:(type_subst (x, ty1)) tys)
  | TyFun (tys, ty) ->
     TyFun (List.map ~f:(type_subst (x, ty1)) tys, type_subst (x, ty1) ty)
  | TyUnion (ty_a, ty_b) ->
     TyUnion(type_subst (x, ty1) ty_a, type_subst (x, ty1) ty_b)
  | TyConstraint (ty, c) ->
     TyConstraint(type_subst (x, ty1) ty, type_subst_to_constraint (x, ty1) c)
  | TyAny -> TyAny
  | TyNone -> TyNone
  | TyInteger -> TyInteger
  | TyAtom -> TyAtom
  | TyConstant const -> TyConstant const
  | TyVar (v:ty_var) ->
     if
       String.equal x v
     then ty1 else (TyVar v)
and type_subst_to_constraint (x, ty1) = function
  | _ ->
     failwith "not implemented "
let solve_sub sol ty1 ty2 = Ok sol (*TODO*)

let rec solve sol = function
  | Empty -> Ok sol
  | Eq (ty1, ty2) ->
     solve_eq sol ty1 ty2
  | Subtype (ty1, ty2) ->
     solve_sub sol ty1 ty2
  | Conj cs ->
     solve_conj sol cs
  | Disj cs ->
     Error (Failure "not implemented : solving Disjunction of constraints")
and solve_conj sol = function
  | [] -> Ok sol
  | c :: cs ->
     let open Result in
     solve sol c >>= fun sol' ->
     solve_conj sol' cs
