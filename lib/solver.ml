open Ast_intf
module List = Base.List
module Map = Base.Map
module String = Base.String
module Result = Base.Result
module Option = Base.Option

type sol = typ Map.M(Type_variable).t
[@@deriving sexp_of]

let string_of_sol sol =
  [%sexp_of: sol] sol |> Sexplib.Sexp.to_string_hum ~indent:2

let init : sol = Map.empty (module Type_variable)

(* type_subst (X, τ_1) τ_2 := [X ↦ τ_1]τ_2 *)
let rec type_subst (x, ty1): typ -> typ = function
  | TyStruct tys ->
     TyStruct(List.map ~f:(type_subst (x, ty1)) tys)
  | TyFun (tys, ty) ->
     TyFun (List.map ~f:(type_subst (x, ty1)) tys, type_subst (x, ty1) ty)
  | TyUnion (ty_a, ty_b) ->
     TyUnion(type_subst (x, ty1) ty_a, type_subst (x, ty1) ty_b)
  | TyConstraint (ty, c) -> (* TODO: for constraint? *)
     TyConstraint(type_subst (x, ty1) ty, c)
  | TyAny -> TyAny
  | TyNone -> TyNone
  | TyInteger -> TyInteger
  | TyAtom -> TyAtom
  | TyConstant const -> TyConstant const
  | TyVar v ->
     if x = v then ty1 else (TyVar v)
and type_subst_to_constraint (x, ty1) = function
  | _ ->
     failwith "not implemented type_subst_to_constraint"

let type_subst_to_sol (x, ty) sol =
  Map.map ~f:(type_subst (x, ty)) sol

let add (x, ty) sol =
  let sol' = type_subst_to_sol (x, ty) sol in
  Map.add_exn sol' ~key:x ~data:ty

let set (x, ty) sol =
  let sol' = type_subst_to_sol (x, ty) sol in
  Map.set sol' ~key:x ~data:ty

let rec is_free x = function
  | TyVar y -> (x <> y)
  | TyStruct tys ->
     List.for_all ~f:(is_free x) tys
  | TyFun (tys, ty) ->
     List.for_all ~f:(is_free x) (ty::tys)
  | TyUnion (ty1, ty2) ->
     is_free x ty1 && is_free x ty2
  | TyConstraint (ty, c) ->
     is_free x ty (* TODO: for constraint ? *)
  | TyAny | TyNone | TyInteger | TyAtom | TyConstant _  -> true

(* see TAPL https://dwango.slack.com/messages/CD453S78B/ *)
let rec solve_eq sol ty1 ty2 =
  match (ty1, ty2) with
  | (ty1, ty2) when ty1 = ty2 -> Ok sol
  | (TyVar x, ty2) when is_free x ty2 ->
     Ok (add (x, ty2) sol)
  | (ty1, TyVar y) when is_free y ty1 ->
     Ok (add (y, ty1) sol)
  | (TyVar v, _) | (_, TyVar v) ->
     Error (Failure (Printf.sprintf "not free variable: %s" (Type_variable.show v)))
  | (TyStruct tys1, TyStruct tys2) when List.length tys1 = List.length tys2 ->
     let open Result in
     List.zip_exn tys1 tys2
     |> List.fold_left ~init:(Ok sol) ~f:(fun res (ty1,ty2) -> res >>= fun sol -> solve_eq sol ty1 ty2)
  | (TyStruct tys1, TyStruct tys2) ->
     Error (Failure ("the tuple types are not different length"))
  | (TyFun (tys1, ty1), TyFun (tys2, ty2)) when List.length tys1 = List.length tys2 ->
     let open Result in
     List.zip_exn (ty1::tys1) (ty2::tys2)
     |> List.fold_left ~init:(Ok sol) ~f:(fun res (ty1,ty2) -> res >>= fun sol -> solve_eq sol ty1 ty2)
  | (TyFun (tys1, ty1), TyFun (tys2, ty2)) ->
     Error (Failure ("the function types's arugment are not different length"))
  | (TyUnion (ty11, ty12), TyUnion (ty21, ty22)) ->
  (* TODO: equality of unions *)
     Error (Failure ("not implemented: solve_eq with union types"))
  | (TyConstraint (ty1, c1), TyConstraint (ty2, c2)) ->
     (* TODO: equality with constraint *)
     Error (Failure ("not implemented: solve_eq with constraint"))
  | _ ->
     Error (Failure (Printf.sprintf "must not eq type: %s =? %s" (show_typ ty1) (show_typ ty2)))

(* τ_1 ⊓ τ_2 *)
let rec meet sol ty1 ty2 =
  match (ty1, ty2) with
  (* var *)
  | TyVar x, _ ->
     let ty1 = Option.value (Map.find sol x) ~default:TyAny in
     meet sol ty1 ty2
  | _, TyVar y ->
     let ty2 = Option.value (Map.find sol y) ~default:TyAny in
     meet sol ty1 ty2
  (* any *)
  | TyAny, _ -> ty2
  | _, TyAny -> ty1
  (* none *)
  | TyNone, _ -> TyNone
  | _, TyNone -> TyNone
  (* struct *)
  | TyStruct tys1, TyStruct tys2 when List.length tys1 = List.length tys2 ->
     List.zip_exn tys1 tys2
     |> List.map ~f:(fun (ty1, ty2) -> meet sol ty1 ty2)
     |> (fun tys -> TyStruct tys)
  (* function *)
  | TyFun (args1, body1), TyFun (args2, body2) when List.length args1 = List.length args2 ->
     List.zip_exn args1 args2
     (* NOTE: using `meet` is the same as type derivation [ABS]. perhaps it should be `join` *)
     |> List.map ~f:(fun (arg1, arg2) -> meet sol arg1 arg2)
     |> (fun args -> TyFun (args, meet sol body1 body2))
  (* union *)
  | TyUnion (tyl, tyr), _ ->
     TyUnion (meet sol tyl ty2, meet sol tyr ty2)
  | _, TyUnion (tyl, tyr) ->
     TyUnion (meet sol ty1 tyl, meet sol ty1 tyr)
  (* constraint *)
  | TyConstraint (ty, c), _ ->
     TyConstraint (meet sol ty ty2, c)
  | _, TyConstraint (ty, c) ->
     TyConstraint (meet sol ty1 ty, c)
  (* integer *)
  | TyInteger, TyInteger -> TyInteger
  | TyInteger, TyConstant (Int n) -> TyConstant (Int n)
  | TyConstant (Int n), TyInteger -> TyConstant (Int n)
  | TyConstant (Int n), TyConstant (Int m) when n = m -> TyConstant (Int n)
  (* atom *)
  | TyAtom, TyAtom -> TyAtom
  | TyConstant (Atom a), TyAtom -> TyConstant (Atom a)
  | TyAtom, TyConstant (Atom a) -> TyConstant (Atom a)
  | TyConstant (Atom a), TyConstant (Atom b) when a = b -> TyConstant (Atom a)
  (* constant *)
  | TyConstant (Float n), TyConstant (Float m) when n = m -> TyConstant (Float n)
  | TyConstant (String s), TyConstant (String t) when s = t -> TyConstant (String s)
  (* otherwise *)
  | _ -> TyNone

(* τ_1 ⊆ τ_2 *)
let is_subtype sol ty1 ty2 =
  meet sol ty1 ty2 = ty1

let rec solve_sub sol ty1 ty2 =
  match (ty1, ty2) with
  | _, TyAny ->
     Ok sol
  | TyVar v1, _ ->
     let ty1' = Option.value (Map.find sol v1) ~default:TyAny in
     if is_subtype sol ty1' ty2 then
       Ok sol
     else
       let ty = meet sol ty1' ty2 in
       if ty != TyNone then
         Ok (set (v1, ty) sol)
       else
         Error (Failure "there is no solution")
  | TyStruct tys1, TyStruct tys2 when List.length tys1 = List.length tys2 ->
     let open Result in
     List.zip_exn tys1 tys2
     |> List.fold_left ~init:(Ok sol) ~f:(fun acc (ty1, ty2) -> acc >>= fun sol -> solve_sub sol ty1 ty2)
  | TyStruct tys1, TyStruct tys2 ->
     Error (Failure "the tuple types are not different length")
  | TyFun (args1, body1), TyFun (args2, body2) when List.length args1 = List.length args2 ->
     let open Result in
     (* NOTE: `solve_sub arg1 arg2` is the same as type derivation [ABS]. *)
     (* perhaps it should be `solve_sub arg2 arg1` *)
     List.zip_exn (body1 :: args1) (body2 :: args2)
     |> List.fold_left ~init:(Ok sol) ~f:(fun acc (ty1, ty2) -> acc >>= fun sol -> solve_sub sol ty1 ty2)
  | TyFun (args1, _), TyFun (args2, _) ->
     Error (Failure "the fun args are not different length")
  | _ ->
     (* TODO: support the other cases *)
     Ok sol

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
