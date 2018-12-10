open Ast_intf
module List = Base.List
module Map = Base.Map
module String = Base.String
module Result = Base.Result
module Option = Base.Option

type solution = typ Map.M(Type_variable).t
[@@deriving sexp_of]

let string_of_sol sol =
  [%sexp_of: solution] sol |> Sexplib.Sexp.to_string_hum ~indent:2

let init : solution = Map.empty (module Type_variable)

(* type_subst (X, τ_1) τ_2 := [X ↦ τ_1]τ_2 *)
let rec type_subst (x, ty1): typ -> typ = function
  | TyTuple tys ->
     TyTuple(List.map ~f:(type_subst (x, ty1)) tys)
  | TyFun (tys, ty) ->
     TyFun (List.map ~f:(type_subst (x, ty1)) tys, type_subst (x, ty1) ty)
  | TyUnion (ty_a, ty_b) ->
     TyUnion(type_subst (x, ty1) ty_a, type_subst (x, ty1) ty_b)
  | TyAny -> TyAny
  | TyBottom -> TyBottom
  | TyNumber -> TyNumber
  | TyAtom -> TyAtom
  | TySingleton const -> TySingleton const
  | TyVar v ->
     if x = v then ty1 else (TyVar v)

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
  | TyTuple tys ->
     List.for_all ~f:(is_free x) tys
  | TyFun (tys, ty) ->
     List.for_all ~f:(is_free x) (ty::tys)
  | TyUnion (ty1, ty2) ->
     is_free x ty1 && is_free x ty2
  | TyAny | TyBottom | TyNumber | TyAtom | TySingleton _  -> true

let rec solve_eq sol ty1 ty2 =
  match (ty1, ty2) with
  | (ty1, ty2) when ty1 = ty2 -> Ok sol
  | (TyVar x, ty2) when is_free x ty2 ->
     begin match Map.find sol x with
     | Some ty' ->
        solve_eq sol ty' ty2
     | None ->
        Ok (add (x, ty2) sol)
     end
  | (ty1, TyVar y) when is_free y ty1 ->
     begin match Map.find sol y with
     | Some ty' ->
        solve_eq sol ty1 ty'
     | None ->
        Ok (add (y, ty1) sol)
     end
  | (TyVar v, _) | (_, TyVar v) ->
     Error (Failure (Printf.sprintf "not free variable: %s" (Type_variable.show v)))
  | (TyTuple tys1, TyTuple tys2) when List.length tys1 = List.length tys2 ->
     let open Result in
     List.zip_exn tys1 tys2
     |> List.fold_left ~init:(Ok sol) ~f:(fun res (ty1,ty2) -> res >>= fun sol -> solve_eq sol ty1 ty2)
  | (TyTuple tys1, TyTuple tys2) ->
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
  | TyBottom, _ -> TyBottom
  | _, TyBottom -> TyBottom
  (* struct *)
  | TyTuple tys1, TyTuple tys2 when List.length tys1 = List.length tys2 ->
     List.zip_exn tys1 tys2
     |> List.map ~f:(fun (ty1, ty2) -> meet sol ty1 ty2)
     |> (fun tys -> TyTuple tys)
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
  (* number *)
  | TyNumber, TyNumber -> TyNumber
  | TyNumber, TySingleton (Number n) -> TySingleton (Number n)
  | TySingleton (Number n), TyNumber -> TySingleton (Number n)
  | TySingleton (Number n), TySingleton (Number m) when n = m -> TySingleton (Number n)
  (* atom *)
  | TyAtom, TyAtom -> TyAtom
  | TySingleton (Atom a), TyAtom -> TySingleton (Atom a)
  | TyAtom, TySingleton (Atom a) -> TySingleton (Atom a)
  | TySingleton (Atom a), TySingleton (Atom b) when a = b -> TySingleton (Atom a)
  (* otherwise *)
  | _ -> TyBottom

(* τ_1 ⊆ τ_2 *)
let is_subtype sol ty1 ty2 =
  meet sol ty1 ty2 = ty1

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
and solve_sub sol ty1 ty2 =
  match (ty1, ty2) with
  | TyVar v1, _ ->
     let ty1' = Option.value (Map.find sol v1) ~default:TyAny in
     if is_subtype sol ty1' ty2 then
       Ok sol
     else
       let ty = meet sol ty1' ty2 in
       if ty != TyBottom then
         Ok (set (v1, ty) sol)
       else
         Error (Failure "there is no solution that satisfies subtype constraints")
  | TyTuple tys1, TyTuple tys2 when List.length tys1 = List.length tys2 ->
     let open Result in
     List.zip_exn tys1 tys2
     |> List.fold_left ~init:(Ok sol) ~f:(fun acc (ty1, ty2) -> acc >>= fun sol -> solve_sub sol ty1 ty2)
  | TyTuple tys1, TyTuple tys2 ->
     Error (Failure "the tuple types are not different length")
  | TyFun (args1, body1), TyFun (args2, body2) when List.length args1 = List.length args2 ->
     let open Result in
     (* NOTE: `solve_sub arg1 arg2` is the same as type derivation [ABS]. *)
     (* perhaps it should be `solve_sub arg2 arg1` *)
     List.zip_exn (body1 :: args1) (body2 :: args2)
     |> List.fold_left ~init:(Ok sol) ~f:(fun acc (ty1, ty2) -> acc >>= fun sol -> solve_sub sol ty1 ty2)
  | TyFun (args1, _), TyFun (args2, _) ->
     Error (Failure "the fun args are not different length")
  | TyUnion (ty11, ty12), _ ->
     let open Result in
     solve_sub sol ty11 ty2 >>= fun sol' -> solve_sub sol' ty12 ty2
  | _ ->
     (* TODO: normalize *)
     if is_subtype sol ty1 ty2 then
       Ok sol
     else
       Error (Failure "there is no solution that satisfies subtype constraints")
