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

let solve_sub sol ty1 ty2 =
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

let solve_eq sol ty1 ty2 =
  let open Result in
  solve_sub sol ty1 ty2 >>= fun sol' ->
  solve_sub sol' ty2 ty1

let merge_solutions sol1 sol2 =
  let f ~key = function
    | `Left ty1  -> Some ty1
    | `Right ty2 -> Some ty2
    | `Both (ty1, ty2) when ty1 = ty2 -> Some ty1
    | `Both (ty1, ty2) -> Some (sup ty1 ty2)
  in
  Map.merge sol1 sol2 ~f

let merge_errors errs =
  let open Known_error in
  let rec iter store = function
    | [] -> `AllTypeError (List.rev store)
    | FialyzerError (TypeError type_errors) :: errors ->
       iter (type_errors :: store) errors
    | FialyzerError (NotImplemented _ as err) :: _ ->
       `NotImplementedExists err
    | FialyzerError InvalidUsage :: _ | FialyzerError (NoSuchFile _) :: _
      | FialyzerError (InvalidBeam _) :: _ | FialyzerError (UnboundVariable _) :: _ ->
       failwith "cannot reach here"
    | other :: _ -> (* general errors *)
       `GeneralError other
  in
  match iter [] errs with
  | `AllTypeError type_errors ->
     FialyzerError (TypeError (List.concat type_errors))
  | `NotImplementedExists err ->
     FialyzerError err
  | `GeneralError exn ->
     exn

let rec solve1 sol = function
  | C.Empty -> Ok sol
  | C.Eq {lhs=ty1; rhs=ty2} ->
     solve_eq sol ty1 ty2
  | C.Subtype {lhs=ty1; rhs=ty2} ->
     solve_sub sol ty1 ty2
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
  let results = List.map ~f:(solve1 sol) cs in
  let valid_solutions = List.filter_map ~f:Result.ok results in
  match valid_solutions with
  | [] -> (* the all elements in results are error *)
     let errors = List.map ~f:(function Error e -> e | _ -> failwith "cannot reach here") results in
     Error (merge_errors errors)
  | sols ->
     Ok (List.reduce_exn ~f:merge_solutions sols)

(*
  After calculating success typing, it is finally checked whether all branches pass type checking.
  Note that this process is outside of the success typing algorithm.
 *)
let rec find_error_clauses sol = function
  | C.Empty -> []
  | C.Eq {lhs=ty1; rhs=ty2} ->
     find_error_clauses sol (Subtype {lhs=ty1; rhs=ty2})
     @ find_error_clauses sol (Subtype {lhs=ty2; rhs=ty1})
  | C.Subtype {lhs=ty1; rhs=ty2} ->
     begin match solve_sub sol ty1 ty2 with
     | Ok _ -> []
     | Error e -> [e]
     end
  | C.Disj cs ->
     List.map ~f:(find_error_clauses sol) cs
     |> List.concat
  | Conj cs ->
     find_conj sol cs
and find_conj sol cs =
  let f sol' c =
    match (find_error_clauses sol' c, solve1 sol' c) with
    | ([], Ok sol'') -> (sol'', [])
    | ([], Error _)  -> failwith "cannot reach here"
    | (es, Ok sol'') -> (sol'', es)
    | (es, Error _)  -> (sol', es)
  in
  List.folding_map cs ~init:sol ~f
  |> List.concat

let rec solve sol cs =
  let open Result in
  solve1 sol cs >>= fun sol' ->
  if Map.equal (=) sol sol' then
    begin match find_error_clauses sol' cs with
    | [] -> Ok sol'
    | _ :: _ as es ->
       Error (merge_errors es)
    end
  else
    solve sol' cs
