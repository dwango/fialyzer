open Type
module List = Base.List
module Map = Base.Map
module String = Base.String
module Result = Base.Result
module Option = Base.Option
open Common

type solution = Type.t Map.M(Type_variable).t
[@@deriving sexp_of]

let string_of_sol sol =
  [%sexp_of: solution] sol |> Sexplib.Sexp.to_string_hum ~indent:2

let init : solution = Map.empty (module Type_variable)

let set (x, ty) sol =
  Map.set sol ~key:x ~data:ty

let rec lookup_type sol = function
  | TyTuple tys ->
     TyTuple(List.map ~f:(lookup_type sol) tys)
  | TyFun (tys, ty) ->
     TyFun (List.map ~f:(lookup_type sol) tys, lookup_type sol ty)
  | TyUnion (ty_a, ty_b) ->
     TyUnion(lookup_type sol ty_a, lookup_type sol ty_b)
  | TyAny -> TyAny
  | TyBottom -> TyBottom
  | TyNumber -> TyNumber
  | TyAtom -> TyAtom
  | TySingleton const -> TySingleton const
  | TyVar v ->
     begin match Map.find sol v with
     | Some v_ty -> v_ty
     | None -> TyAny
     end

(* τ_1 ⊓ τ_2 *)
let rec meet ty1 ty2 =
  match (ty1, ty2) with
  (* var *)
  | TyVar _, _ | _, TyVar _ -> failwith "cannot reach here"
  (* any *)
  | TyAny, _ -> ty2
  | _, TyAny -> ty1
  (* none *)
  | TyBottom, _ -> TyBottom
  | _, TyBottom -> TyBottom
  (* struct *)
  | TyTuple tys1, TyTuple tys2 when List.length tys1 = List.length tys2 ->
     List.zip_exn tys1 tys2
     |> List.map ~f:(fun (ty1, ty2) -> meet ty1 ty2)
     |> (fun tys -> TyTuple tys)
  (* function *)
  | TyFun (args1, body1), TyFun (args2, body2) when List.length args1 = List.length args2 ->
     List.zip_exn args1 args2
     (* NOTE: using `meet` is the same as type derivation [ABS]. perhaps it should be `join` *)
     |> List.map ~f:(fun (arg1, arg2) -> meet arg1 arg2)
     |> (fun args -> TyFun (args, meet body1 body2))
  (* union *)
  | TyUnion (tyl, tyr), _ ->
     TyUnion (meet tyl ty2, meet tyr ty2)
  | _, TyUnion (tyl, tyr) ->
     TyUnion (meet ty1 tyl, meet ty1 tyr)
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
let is_subtype ty1 ty2 =
  meet ty1 ty2 = ty1

let rec solve_sub sol ty1 ty2 =
  let ty1' = lookup_type sol ty1 in
  let ty2' = lookup_type sol ty2 in
  let inf = meet ty1' ty2' in
  match (ty1, inf) with
  | TyVar v1, _ ->
     if is_subtype ty1' ty2' then
       Ok sol
     else
       if inf != TyBottom then
         Ok (set (v1, inf) sol)
       else
         let filename = "TODO:filename" in
         let line = -1 (*TODO:line*) in
         let actual = ty1 in
         let expected = ty2 in
         let message = "there is no solution that satisfies subtype constraints" in
         Error Known_error.(FialyzerError(TypeError {filename; line; actual; expected; message}))
  | TyTuple tys1, TyTuple tys2 when List.length tys1 = List.length tys2 ->
     let open Result in
     List.zip_exn tys1 tys2
     |> List.fold_left ~init:(Ok sol) ~f:(fun acc (ty1, ty2) -> acc >>= fun sol -> solve_sub sol ty1 ty2)
  | TyTuple tys1, TyTuple tys2 ->
     let filename = "TODO:filename" in
     let line = -1 (*TODO:line*) in
     let actual = ty1' in
     let expected = ty2' in
     let message = "the tuple types are not different length" in
     Error Known_error.(FialyzerError(TypeError {filename; line; actual; expected; message}))
  | TyFun (args1, body1), TyFun (args2, body2) when List.length args1 = List.length args2 ->
     let open Result in
     (* NOTE: `solve_sub arg1 arg2` is the same as type derivation [ABS]. *)
     (* perhaps it should be `solve_sub arg2 arg1` *)
     List.zip_exn (body1 :: args1) (body2 :: args2)
     |> List.fold_left ~init:(Ok sol) ~f:(fun acc (ty1, ty2) -> acc >>= fun sol -> solve_sub sol ty1 ty2)
  | TyFun (args1, _), TyFun (args2, _) ->
     let filename = "TODO:filename" in
     let line = -1 (*TODO:line*) in
     let actual = ty1' in
     let expected = ty2' in
     let message = "the fun args are not different length" in
     Error Known_error.(FialyzerError(TypeError {filename; line; actual; expected; message}))
  | TyUnion (ty11, ty12), _ ->
     let open Result in
     solve_sub sol ty11 ty2 >>= fun sol' -> solve_sub sol' ty12 ty2
  | _ ->
     (* TODO: normalize *)
     if is_subtype ty1' ty2' then
       Ok sol
     else
       let filename = "TODO:filename" in
       let line = -1 (*TODO:line*) in
       let actual = ty1' in
       let expected = ty2' in
       let message = !%"there is no solution that satisfies subtype constraints" in
       Error Known_error.(FialyzerError(TypeError {filename; line; actual; expected; message}))

let solve_eq sol ty1 ty2 =
  let open Result in
  solve_sub sol ty1 ty2 >>= fun sol' ->
  solve_sub sol' ty2 ty1

let rec solve1 sol = function
  | Empty -> Ok sol
  | Eq (ty1, ty2) ->
     solve_eq sol ty1 ty2
  | Subtype (ty1, ty2) ->
     solve_sub sol ty1 ty2
  | Conj cs ->
     solve_conj sol cs
  | Disj cs ->
     let issue_links = ["https://github.com/dwango/fialyzer/issues/99"] in
     let message = "Solve disjunction of constraints" in
     Error Known_error.(FialyzerError (NotImplemented {issue_links; message}))
and solve_conj sol = function
  | [] -> Ok sol
  | c :: cs ->
     let open Result in
     solve1 sol c >>= fun sol' ->
     solve_conj sol' cs

let rec solve sol cs =
  let open Result in
  solve1 sol cs >>= fun sol' ->
  if Map.equal (=) sol sol' then
    Ok sol'
  else
    solve sol' cs
