open Type
module List = Base.List
module Map = Base.Map
module String = Base.String
module Result = Base.Result
module Option = Base.Option
module C = Constraint
open Common

type solution = Type.t Map.M(Type_variable).t
[@@deriving sexp_of]

let string_of_sol sol =
  Map.to_alist sol
  |> List.map ~f:(fun (k,v) -> (!%"%s |-> %s" (Type_variable.show k) (Type.pp v)))
  |> String.concat ~sep:"\n"

let init : solution = Map.empty (module Type_variable)

let set (x, ty) sol =
  Map.set sol ~key:x ~data:ty

let rec lookup_type sol = function
  | TyAny -> TyAny
  | TyBottom -> TyBottom
  | TyUnion tys ->
     List.map ~f:(lookup_elem sol) tys
     |> List.reduce_exn ~f:Type.sup
and lookup_elem sol = function
  | TyTuple tys ->
     TyUnion [TyTuple(List.map ~f:(lookup_type sol) tys)]
  | TyFun (tys, ty) ->
     TyUnion [TyFun (List.map ~f:(lookup_type sol) tys, lookup_type sol ty)]
  | TyNumber -> TyUnion [TyNumber]
  | TyAtom -> TyUnion [TyAtom]
  | TySingleton const -> TyUnion [TySingleton const]
  | TyVar v ->
     begin match Map.find sol v with
     | Some v_ty -> v_ty
     | None -> TyAny
     end

(**
  τ_1 ⊆ τ_2
  assume no type variable in [ty1] and [ty2]
 *)
let is_subtype ty1 ty2 =
  Type.inf ty1 ty2 = ty1

(**
  assume no type variable in the 2nd argument [inf]
 *)
let rec unify ty inf =
  match ty with
  | TyUnion [elem] ->
     unify_elem elem inf
  | TyUnion _ -> [] (* TODO:??? *)
  | TyAny | TyBottom -> []
and unify_elem elem inf =
  match (elem, inf) with
  | (TyNumber, _) | (TyAtom, _) | (TySingleton _, _) -> []
  | (TyVar v, _) ->
     [(v, inf)]
  | (TyTuple tys1, TyUnion [TyTuple tys2]) when List.length tys1 = List.length tys2 ->
     List.map2_exn ~f:unify tys1 tys2
     |> List.concat
  | TyFun (args1, body1), TyUnion [TyFun (args2, body2)] when List.length args1 = List.length args2 ->
     List.map2_exn ~f:unify (body1::args1) (body2::args2)
     |> List.concat
  | _ -> []

let solve_sub expr sol ty1 ty2 =
  let ty1' = lookup_type sol ty1 in
  let ty2' = lookup_type sol ty2 in
  let inf = inf ty1' ty2' in
  if is_subtype ty1' ty2' then
    Ok sol
  else if inf = TyBottom then
    let filename = "TODO:filename" in
    let line = -1 (*TODO:line*) in
    let actual = ty1' in
    let expected = ty2' in
    let message = !%"there is no solution that satisfies subtype constraints" in
    Error Known_error.(FialyzerError(TypeError [{filename; line; actual; expected; message}]))
  else
    unify ty1 inf
    |> List.fold_left ~f:(fun sol (v,ty) -> set (v, ty) sol) ~init:sol
    |> Result.return

let solve_eq expr sol ty1 ty2 =
  let open Result in
  solve_sub expr sol ty1 ty2 >>= fun sol' ->
  solve_sub expr sol' ty2 ty1

let merge_solutions sol1 sol2 =
  let f ~key = function
    | `Left ty1  -> Some ty1
    | `Right ty2 -> Some ty2
    | `Both (ty1, ty2) when ty1 = ty2 -> Some ty1
    | `Both (ty1, ty2) -> Some (sup ty1 ty2)
  in
  Map.merge sol1 sol2 ~f

let rec solve1 sol = function
  | C.Empty -> Ok sol
  | C.Eq {lhs=ty1; rhs=ty2; link} ->
     solve_eq link sol ty1 ty2
  | C.Subtype {lhs=ty1; rhs=ty2; link} ->
     solve_sub link sol ty1 ty2
  | C.Conj cs ->
     solve_conj sol cs
  | C.Disj cs ->
     solve_disj sol cs
and solve_conj sol = function
  | [] -> Ok sol
  | c :: cs ->
     let open Result in
     solve1 sol c >>= fun sol' ->
     solve_conj sol' cs
and solve_disj sol cs =
  let open Result in
  result_map_m ~f:(solve1 sol) cs >>= fun sols ->
  Ok (List.reduce_exn ~f:merge_solutions sols)

let rec solve sol cs =
  let open Result in
  solve1 sol cs >>= fun sol' ->
  if Map.equal (=) sol sol' then
    Ok sol'
  else
    solve sol' cs
