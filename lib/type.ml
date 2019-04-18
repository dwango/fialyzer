open Base
open Poly
module F = Obeam.Abstract_format
open Common

type t =
    | TyUnion of t_union_elem list (* has one or more elements *)
    | TyAny
    | TyBottom
 and t_union_elem =
    | TyNumber
    | TyAtom
    | TySingleton of Constant.t
    | TyVar of Type_variable.t
    | TyTuple of t list
    | TyList of t
    | TyFun of t list * t
    | TyAnyMap
[@@deriving sexp_of]

(* ref: http://erlang.org/doc/reference_manual/typespec.html *)
let rec pp = function
  | TyUnion tys ->
     List.map ~f:pp_t_union_elem tys |> String.concat ~sep:" | "
  | TyAny -> "any()"
  | TyBottom -> "none()"
and pp_t_union_elem = function
  | TyNumber -> "number()"
  | TyAtom -> "atom()"
  | TySingleton c -> Constant.pp c
  | TyVar var -> Type_variable.to_string var
  | TyTuple ts -> "{" ^ (ts |> List.map ~f:pp |> String.concat ~sep:", ") ^ "}"
  | TyList t -> "[" ^ (pp t) ^ "]"
  | TyFun (args, ret) ->
     let args_str = "(" ^ (args |> List.map ~f:pp |> String.concat ~sep:", ") ^ ")" in
     let ret_str = pp ret in
     "fun(" ^ args_str ^ " -> " ^ ret_str ^ ")"
  | TyAnyMap -> "map()"

let bool = TyUnion [TySingleton (Atom "true"); TySingleton (Atom "false")]
let of_elem e = TyUnion [e]

let rec variables = function
  | TyAny
  | TyBottom -> []
  | TyUnion es ->
     List.concat_map ~f:variables_elem es
and variables_elem = function
  | TyNumber
  | TyAtom
  | TySingleton _
  | TyAnyMap -> []
  | TyList t ->
    variables t
  | TyTuple ts ->
     List.concat_map ~f:variables ts
  | TyFun (args, ret) ->
     List.concat_map ~f:variables (args @ [ret])
  | TyVar v -> [v]

(**
   supremum of two types: ty1 ∪ ty2
   assume no type variable in the arguments
 *)
let rec sup ty1 ty2 =
  match (ty1, ty2) with
  | _ when ty1 = ty2 -> ty1
  | (TyAny, _) | (_, TyAny) -> TyAny
  | (TyBottom, ty) | (ty, TyBottom) -> ty
  | (TyUnion tys1, TyUnion tys2) ->
     TyUnion (sup_elems_to_list tys2 (List.rev tys1)) (* has one or more element *)
and sup_elems_to_list store = function
  | [] -> store
  | TyVar v1 :: ty1s when List.exists ~f:(function TyVar v2 -> v1 = v2 | _ -> false) store ->
    sup_elems_to_list store ty1s
  | TyVar v1 :: ty1s ->
    sup_elems_to_list ((TyVar v1) :: store) ty1s
  | ty1 :: ty1s when List.exists ~f:((=) ty1) store ->
     sup_elems_to_list store ty1s
  | TyNumber :: ty1s ->
     let is_not_number = function TySingleton (Number _) -> false | _ -> true in
     let store' = TyNumber :: List.filter ~f:is_not_number store in
     sup_elems_to_list store' ty1s
  | TySingleton (Number n) :: ty1s when List.exists ~f:((=) TyNumber) store ->
     sup_elems_to_list store ty1s
  | TySingleton (Number n) :: ty1s ->
     sup_elems_to_list (TySingleton (Number n) :: store) ty1s
  | TyAtom :: ty1s ->
     let is_not_atom = function TySingleton (Atom _) -> false | _ -> true in
     let store' = TyAtom :: List.filter ~f:is_not_atom store in
     sup_elems_to_list store' ty1s
  | TySingleton (Atom a) :: ty1s when List.exists ~f:((=) TyAtom) store ->
     sup_elems_to_list store ty1s
  | TySingleton (Atom a) :: ty1s ->
     sup_elems_to_list (TySingleton (Atom a) :: store) ty1s
  | TyList ty2 :: ty1s ->
    let store' =
       if List.exists ~f:(function TyList ty -> true | _ -> false) store then
         store |> List.map ~f:(function
         | TyList ty1 -> TyList (sup ty1 ty2)
         | t -> t
         )
       else
         TyList ty2 :: store
    in
    sup_elems_to_list store' ty1s
  | TyTuple ty2s :: ty1s ->
     let store' =
       if List.exists ~f:(function TyTuple tys when List.length ty2s = List.length tys -> true | _ -> false) store then
         List.map ~f:(function
                      | TyTuple tys when List.length ty2s = List.length tys ->
                         List.map2_exn ~f:sup tys ty2s
                         |> fun ty2s' -> TyTuple ty2s'
                      | t -> t)
                  store
       else
         TyTuple ty2s :: store
     in
     sup_elems_to_list store' ty1s
  | TyFun (args, range) :: ty1s ->
     let store' =
       if List.exists ~f:(function TyFun (args0, _) when List.length args0 = List.length args -> true | _ -> false) store then
         List.map ~f:(function
                      | TyFun (args0, range0) when List.length args0 = List.length args ->
                         List.map2_exn ~f:sup args0 args
                         |> fun ty2s' -> TyFun (ty2s', sup range range0)
                      | t -> t)
                  store
       else
         TyFun (args, range) :: store
     in
     sup_elems_to_list store' ty1s
  | TyAnyMap :: ty1s ->
     let is_not_any_map = function TyAnyMap -> false | _ -> true in
     let store' = TyAnyMap :: List.filter ~f:is_not_any_map store in
     sup_elems_to_list store' ty1s

let union_list tys =
  List.reduce_exn ~f:sup tys

(**
   infimum of two types: ty1 ∩ ty2
   assume no type variable in the arguments
 *)
let rec inf ty1 ty2 =  (* ty1 and ty2 should be a TyAny, TyBottom, TyVar or TyUnion *)
  match (ty1, ty2) with
  | _ when ty1 = ty2 -> ty1
  | (TyAny, ty) | (ty, TyAny) -> ty
  | (TyBottom, _) | (_, TyBottom) -> TyBottom
  | (TyUnion tys1, TyUnion tys2) ->
     let ty1s =
       List.cartesian_product tys1 tys2
       |> List.filter_map ~f:(fun (ty1, ty2) -> inf_elem ty1 ty2)
     in
     begin match sup_elems_to_list [] ty1s with
     | [] -> TyBottom
     | ty1s -> TyUnion ty1s
     end
and inf_elem ty1 ty2 =  (* ty1 and ty2 should be a TyNumber, TySingleton, TyAtom, TyTuple, TyList or TyFun *)
  match (ty1, ty2) with
  | (TyVar _, _) | (_, TyVar _) -> failwith "cannot reach here"
  | _ when ty1 = ty2 -> Some ty1
  | (TyNumber, TySingleton (Number n)) | (TySingleton (Number n), TyNumber) ->
     Some (TySingleton (Number n))
  | (TyAtom, TySingleton (Atom a)) | (TySingleton (Atom a), TyAtom) ->
     Some (TySingleton (Atom a))
  | (TyTuple tys1, TyTuple tys2) when List.length tys1 = List.length tys2 ->
     List.map2_exn ~f:inf tys1 tys2
     |> fun tys ->
        if List.exists ~f:(fun t -> t = TyBottom) tys then
          None
        else
          Some (TyTuple tys)
  | (TyList t1, TyList t2) ->
    Some (TyList (inf t1 t2))
  | (TyFun (args1, range1), TyFun (args2, range2)) when List.length args1 = List.length args2 ->
     let args' = List.map2_exn ~f:inf args1 args2 in
     let range' = inf range1 range2 in
     if List.exists ~f:(fun t -> t = TyBottom) (range' :: args') then
       None
     else
       Some (TyFun (args', range'))
  | (_, _) ->
     None

let rec subst (v, ty) = function
  | TyAny -> TyAny
  | TyBottom -> TyBottom
  | TyUnion tys ->
     List.map ~f:(subst_elem (v, ty)) tys
     |> List.reduce_exn ~f:sup
and subst_elem (v, ty0) = function
  | TyTuple tys ->
     TyUnion [TyTuple(List.map ~f:(subst (v,ty0)) tys)]
  | TyList t -> TyUnion [TyList (subst (v, ty0) t)]
  | TyFun (tys, ty) ->
     TyUnion [TyFun (List.map ~f:(subst (v,ty0)) tys, subst (v,ty0) ty)]
  | TyNumber -> TyUnion [TyNumber]
  | TyAtom -> TyUnion [TyAtom]
  | TySingleton const -> TyUnion [TySingleton const]
  | TyVar x when x = v ->
     ty0
  | TyVar x ->
     of_elem (TyVar x)
  | TyAnyMap -> TyUnion [TyAnyMap]

let solve_constraints_map map =
  let subst_all (v, expr) l =
    List.Assoc.map ~f:(subst (v, expr)) l
  in
  let rec iter store = function
    | [] -> List.rev store
    | (v, expr) :: rest ->
      let store' = (v, expr) :: subst_all (v, expr) store in
      iter store' (subst_all (v, expr) rest)
  in
  iter [] map

let rec of_absform = function
  | F.TyAnn {tyvar; _} -> of_absform tyvar
  | F.TyLit {lit=LitAtom {atom; _}} -> of_elem (TySingleton (Atom atom))
  | F.TyLit {lit=LitInteger {integer; _}} -> of_elem (TySingleton (Number (Int integer)))
  | F.TyPredef {name="any"; args=[]; _} | F.TyPredef {name="term"; args=[]; _} ->
     TyAny
  | F.TyPredef {name="none"; args=[]; _} ->
     TyBottom
  | F.TyPredef {name="atom"; args=[]; _} ->
     of_elem TyAtom
  | F.TyPredef {name="number"; args=[]; _}
  | F.TyPredef {name="float"; args=[]; _}
  | F.TyPredef {name="integer"; args=[]; _} ->
     of_elem TyNumber
  | F.TyPredef {name="integer"; args=[]; _}
  | F.TyPredef {name="non_neg_integer"; args=[]; _}
  | F.TyPredef {name="pos_integer"; args=[]; _}
  | F.TyPredef {name="neg_integer"; args=[]; _} ->
     of_elem TyNumber
  | F.TyPredef {name="boolean"; args=[]; _} ->
     bool
  | F.TyPredef {name="nil"; args=[]; _} ->
     of_elem (TyList TyBottom)
  | F.TyPredef {name="list"; args=[]; _}
  | F.TyPredef {name="maybe_improper_list"; args=[]; _}
  | F.TyPredef {name="nonempty_list"; args=[]; _} ->
     of_elem (TyList TyAny)
  | F.TyPredef {name="list"; args=[ty]; _}
  | F.TyPredef {name="maybe_improper_list"; args=[ty; _]; _}
  | F.TyPredef {name="nonempty_list"; args=[ty]; _} ->
     of_elem (TyList (of_absform ty))
  | F.TyVar {id; _} -> of_elem (TyVar (Type_variable.of_string id))
  | F.TyFun {params; ret; _} ->
     of_elem (TyFun(List.map ~f:of_absform params, of_absform ret))
  | F.TyContFun {function_type; constraints; _} ->
     let map_of_constraints = function
       | F.TyCont {constraints=cs} ->
         let f = function
           | F.TyContRel {constraint_kind=F.TyContIsSubType _; lhs=TyVar v; rhs; _} ->
             Log.debug [%here] "CONSTRAINT: '%s' -> %s" v.id (F.sexp_of_type_t rhs |> Sexp.to_string_hum);
             (Type_variable.of_string v.id, rhs)
           | F.TyContRel _ | F.TyCont _ | F.TyContIsSubType _ ->
             failwith "cannot rearch here"
         in
         List.map ~f cs
       | F.TyContRel _ | F.TyContIsSubType _ ->
         failwith "cannot rearch here"
     in
     let map =
       map_of_constraints constraints
       |> List.Assoc.map ~f:of_absform
       |> solve_constraints_map
     in
     let ty0 = of_absform function_type in
     List.fold_left ~f:(fun ty (v, t) -> subst (v, t) ty) ~init:ty0 map
  | F.TyTuple {elements=ts; _} ->
     of_elem (TyTuple (List.map ~f:of_absform ts))
  | F.TyUnion {elements; _} ->
     List.map ~f:of_absform elements
     |> union_list
  | F.TyMap _
  | F.TyAnyMap _ -> of_elem TyAnyMap
  | F.TyPredef {name="string"; args=[]; _}
  | F.TyPredef {name="nonempty_string"; args=[]; _} ->
     of_elem (TyList (of_elem TyNumber))
  | F.TyPredef {name="pid"; args=[]; _}
  | F.TyPredef {name="port"; args=[]; _}
  | F.TyPredef {name="reference"; args=[]; _}
  | F.TyPredef {name="binary"; args=[]; _}
  | F.TyPredef {name="bitstring"; args=[]; _}
  | F.TyPredef {name="byte"; args=[]; _}
  | F.TyPredef {name="char"; args=[]; _}
  | F.TyPredef {name="iodata"; args=[]; _}
  | F.TyPredef {name="iolist"; args=[]; _}
  | F.TyPredef {name="function"; args=[]; _}
  | F.TyPredef {name="module"; args=[]; _}
  | F.TyPredef {name="mfa"; args=[]; _}
  | F.TyPredef {name="arity"; args=[]; _}
  | F.TyPredef {name="identifier"; args=[]; _}
  | F.TyPredef {name="node"; args=[]; _}
  | F.TyPredef {name="timeout"; args=[]; _}
  | F.TyPredef {name="no_return"; args=[]; _}
  | F.TyBitstring _
  | F.TyBinOp _
  | F.TyUnaryOp _
  | F.TyRange _
  | F.TyFunAny _
  | F.TyFunAnyArity _
  | F.TyAnyTuple _
  | F.TyUser _
  | F.TyLit _
  | F.TyRecord _
  | F.TyRemote _
    as other ->
     Log.debug [%here] "not implemented conversion from type: %s" (F.sexp_of_type_t other |> Sexp.to_string_hum);
     of_elem (TySingleton (Atom (!%"not_implemented %s" (F.sexp_of_type_t other |> Sexp.to_string_hum))))
  | F.TyPredef {line; name; args} ->
     failwith (!%"Prease report: line:%d: unexpected predef type: '%s/%d'" line name (List.length args))

let rec of_erl_type = function
  | Erl_type.Any -> TyAny
  | None_ -> TyBottom
  | Unit ->
     Log.debug [%here] "not implemented conversion from erl_type: Unit";
     of_elem (TySingleton (Atom "not_implemented"))
  | erl_type -> TyUnion (union_of_erl_type erl_type)
and union_of_erl_type = function
  | Erl_type.None_ -> []
  | AnyAtom -> [TyAtom]
  | AtomUnion atoms -> List.map ~f:(fun atom -> TySingleton (Constant.Atom atom)) atoms
  | Function {params; ret} -> [TyFun (params |> List.map ~f:of_erl_type, of_erl_type ret)]
  | Number (Erl_type.IntSet [n]) -> [TySingleton (Number (Int n))]
  | Number _ -> [TyNumber] (* TODO IntSet with multiple integers *)
  | Var (VAtom var_id) -> [TyVar (Type_variable.of_string var_id)]
  | Tuple {types; _} -> [TyTuple (List.map ~f:of_erl_type types)]
  | NTuplesUnion n_tuples_list ->
      Erl_type.(List.concat_map ~f:(fun {tuples; _} -> List.map ~f:(fun {types; _} -> TyTuple (List.map ~f:of_erl_type types)) tuples) n_tuples_list)
  | Union erl_types -> List.concat_map ~f:union_of_erl_type erl_types
  | AnyMap -> [TyAnyMap]
  | Var (VInt _)
  | Binary _
  | AnyIdentifier
  | IdentifierUnion _
  | List _
  | Nil
  | Map _
  | OpaqueUnion _
  | AnyTuple
    as other ->
     Log.debug [%here] "not implemented conversion from erl_type: %s" (Erl_type.sexp_of_t other |> Sexp.to_string_hum);
     [TySingleton (Atom "not_implemented")]
  | Any | Unit ->
     failwith "cannot reach here"
