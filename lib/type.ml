open Base

type t =
    | TyVar of Type_variable.t
    | TyTuple of t list
    | TyFun of t list * t
    | TyUnion of t list (* all member of the list is not a union *)
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
  | TyUnion tys ->
     List.map ~f:pp tys |> String.concat ~sep:" | "
  | TyAny -> "any()"
  | TyBottom -> "none()"
  | TyNumber -> "number()"
  | TyAtom -> "atom()"
  | TySingleton c -> Constant.pp c

let bool = TyUnion [TySingleton (Atom "true"); TySingleton (Atom "false")]


let rec sup2 ty1 ty2 = (* ty1 and ty2 should be a TyAny, TyBottom, TyVar or TyUnion *)
  match (ty1, ty2) with
  | _ when ty1 = ty2 -> ty1
  | (TyAny, _) | (_, TyAny) -> TyAny
  | (TyBottom, ty) | (ty, TyBottom) -> ty
  | (TyVar _, _) | (_, TyVar _) ->
     failwith "cannotreach"
  | (TyUnion tys1, TyUnion tys2) ->
     begin match sup1_list tys1 tys2 with
     | [] -> TyBottom
     | ty1s -> TyUnion ty1s
     end
  | _ -> failwith "cannot reach here"
and sup1_list store = function (* all member of the list should be a TyNumber, TySingleton, TyAtom, TyTuple or TyFun *)
  | [] -> store
  | ty1 :: ty1s when List.exists ~f:((=) ty1) store ->
     sup1_list store ty1s
  | TyNumber :: ty1s ->
     let is_not_number = function TySingleton (Number _) -> false | _ -> true in
     let store' = TyNumber :: List.filter ~f:is_not_number store in
     sup1_list store' ty1s
  | TySingleton (Number n) :: ty1s when List.exists ~f:((=) TyNumber) store ->
     sup1_list store ty1s
  | TySingleton (Number n) :: ty1s ->
     sup1_list (TySingleton (Number n) :: store) ty1s
  | TyAtom :: ty1s ->
     let is_not_atom = function TySingleton (Atom _) -> false | _ -> true in
     let store' = TyAtom :: List.filter ~f:is_not_atom store in
     sup1_list store' ty1s
  | TySingleton (Atom a) :: ty1s when List.exists ~f:((=) TyAtom) store ->
     sup1_list store ty1s
  | TySingleton (Atom a) :: ty1s ->
     sup1_list (TySingleton (Atom a) :: store) ty1s
  | TyTuple ty2s :: ty1s ->
     let store' =
       if List.exists ~f:(function TyTuple tys when List.length ty2s = List.length tys -> true | _ -> false) store then
         List.map ~f:(function
                      | TyTuple tys when List.length ty2s = List.length tys ->
                         List.map2_exn ~f:sup2 tys ty2s
                         |> fun ty2s' -> TyTuple ty2s'
                      | t -> t)
                  store
       else
         TyTuple ty2s :: store
     in
     sup1_list store' ty1s
  | TyFun (args, range) :: ty1s ->
     let store' =
       if List.exists ~f:(function TyFun (args0, _) when List.length args0 = List.length args -> true | _ -> false) store then
         List.map ~f:(function
                      | TyFun (args0, range0) when List.length args0 = List.length args ->
                         List.map2_exn ~f:sup2 args0 args
                         |> fun ty2s' -> TyTuple ty2s'
                      | t -> t)
                  store
       else
         TyFun (args, range) :: store
     in
     sup1_list store' ty1s
  | _ -> failwith "cannot reach here"

let sup ty1 ty2 =
  let f = function
    | TyNumber | TyAtom | TySingleton _ | TyTuple _ | TyFun _ as t -> TyUnion [t]
    | t -> t
  in
  sup2 (f ty1) (f ty2)

let union_list tys =
  List.reduce_exn ~f:sup tys

let rec inf2 ty1 ty2 =  (* ty1 and ty2 should be a TyAny, TyBottom, TyVar or TyUnion *)
  match (ty1, ty2) with
  | _ when ty1 = ty2 -> ty1
  | (TyAny, ty) | (ty, TyAny) -> ty
  | (TyBottom, _) | (_, TyBottom) -> TyBottom
  | (TyVar _, _) | (_, TyVar _) ->
     failwith "cannotreach"
  | (TyUnion tys1, TyUnion tys2) ->
     let ty1s =
       List.cartesian_product tys1 tys2
       |> List.filter_map ~f:(fun (ty1, ty2) -> inf1 ty1 ty2)
     in
     begin match sup1_list [] ty1s with
     | [] -> TyBottom
     | ty1s -> TyUnion ty1s
     end
  | _ -> failwith "cannot reach here"
and inf1 ty1 ty2 =  (* ty1 and ty2 should be a TyNumber, TySingleton, TyAtom, TyTuple or TyFun *)
  match (ty1, ty2) with
  | _ when ty1 = ty2 -> Some ty1
  | (TyNumber, TySingleton (Number n)) | (TySingleton (Number n), TyNumber) ->
     Some (TySingleton (Number n))
  | (TyAtom, TySingleton (Atom a)) | (TySingleton (Atom a), TyAtom) ->
     Some (TySingleton (Atom a))
  | (TyTuple tys1, TyTuple tys2) when List.length tys1 = List.length tys2 ->
     List.map2_exn ~f:inf2 tys1 tys2
     |> fun tys -> Some (TyTuple tys)
  | (TyFun (args1, range1), TyFun (args2, range2)) ->
     let args' = List.map2_exn ~f:inf2 args1 args2 in
     let range' = inf2 range1 range2 in
     Some (TyFun (args', range'))
  | (TyAny, _) | (_, TyAny) | (TyBottom, _) | (_, TyBottom) | (TyVar _, _) | (_, TyVar _) | (TyUnion _, _) | (_, TyUnion _) ->
     failwith "cannot reach here"
  | (_, _) ->
     None

let inf ty1 ty2 =
  let f = function
    | TyNumber | TyAtom | TySingleton _ | TyTuple _ | TyFun _ as t -> TyUnion [t]
    | t -> t
  in
  match inf2 (f ty1) (f ty2) with
  | TyUnion [t] -> t
  | t -> t
