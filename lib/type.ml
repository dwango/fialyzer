open Base
open Polymorphic_compare
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
    | TyFun of t list * t
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
  | TyFun (args, ret) ->
     let args_str = "(" ^ (args |> List.map ~f:pp |> String.concat ~sep:", ") ^ ")" in
     let ret_str = pp ret in
     "fun(" ^ args_str ^ " -> " ^ ret_str ^ ")"

let bool = TyUnion [TySingleton (Atom "true"); TySingleton (Atom "false")]
let of_elem e = TyUnion [e]

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
  | TyVar _ :: _ -> failwith "cannot reach here"
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
and inf_elem ty1 ty2 =  (* ty1 and ty2 should be a TyNumber, TySingleton, TyAtom, TyTuple or TyFun *)
  match (ty1, ty2) with
  | (TyVar _, _) | (_, TyVar _) -> failwith "cannot reach here"
  | _ when ty1 = ty2 -> Some ty1
  | (TyNumber, TySingleton (Number n)) | (TySingleton (Number n), TyNumber) ->
     Some (TySingleton (Number n))
  | (TyAtom, TySingleton (Atom a)) | (TySingleton (Atom a), TyAtom) ->
     Some (TySingleton (Atom a))
  | (TyTuple tys1, TyTuple tys2) when List.length tys1 = List.length tys2 ->
     List.map2_exn ~f:inf tys1 tys2
     |> fun tys -> Some (TyTuple tys)
  | (TyFun (args1, range1), TyFun (args2, range2)) when List.length args1 = List.length args2 ->
     let args' = List.map2_exn ~f:inf args1 args2 in
     let range' = inf range1 range2 in
     Some (TyFun (args', range'))
  | (_, _) ->
     None

let rec of_absform = function
  | F.TyAnn {tyvar; _} -> of_absform tyvar
  | F.TyLit {lit=LitAtom {atom; _}} -> of_elem (TySingleton (Atom atom))
  | F.TyLit {lit=LitInteger {integer; _}} -> of_elem (TySingleton (Number integer))
  | F.TyPredef {name="any"; args=[]; _} | F.TyPredef {name="term"; args=[]; _} ->
     TyAny
  | F.TyPredef {name="none"; args=[]; _} ->
     TyBottom
  | F.TyPredef {name="atom"; args=[]; _} ->
     of_elem TyAtom
  | F.TyPredef {name="number"; args=[]; _} ->
     of_elem TyNumber
  | F.TyPredef {name="boolean"; args=[]; _} ->
     bool
  | F.TyVar {id; _} -> of_elem (TyVar (Type_variable.of_string id))
  | F.TyFun {params; ret; _} ->
     of_elem (TyFun(List.map ~f:of_absform params, of_absform ret))
  | F.TyTuple {elements=ts; _} ->
     of_elem (TyTuple (List.map ~f:of_absform ts))
  | F.TyUnion {elements; _} ->
     List.map ~f:of_absform elements
     |> union_list
  | F.TyPredef {name="pid"; args=[]; _}
  | F.TyPredef {name="port"; args=[]; _}
  | F.TyPredef {name="reference"; args=[]; _}
  | F.TyPredef {name="float"; args=[]; _}
  | F.TyPredef {name="integer"; args=[]; _}
  | F.TyPredef {name="binary"; args=[]; _}
  | F.TyPredef {name="bitstring"; args=[]; _}
  | F.TyPredef {name="byte"; args=[]; _}
  | F.TyPredef {name="char"; args=[]; _}
  | F.TyPredef {name="nil"; args=[]; _}
  | F.TyPredef {name="list"; args=[]; _}
  | F.TyPredef {name="list"; args=[_]; _}
  | F.TyPredef {name="maybe_improper_list"; args=[]; _}
  | F.TyPredef {name="maybe_improper_list"; args=[_; _]; _}
  | F.TyPredef {name="non_empty_list"; args=[]; _}
  | F.TyPredef {name="non_empty_list"; args=[_]; _}
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
  | F.TyAnyMap _
  | F.TyMap _
  | F.TyFunAny _
  | F.TyFunAnyArity _
  | F.TyContFun _
  | F.TyAnyTuple _
  | F.TyUser _
  | F.TyLit _
    as other ->
     Log.debug [%here] "not implemented conversion from type: %s" (F.sexp_of_type_t other |> Sexp.to_string_hum);
     of_elem (TySingleton (Atom "not_implemented"))
  | F.TyPredef {line; name; args} ->
     failwith (!%"Prease report: line:%d: unexpected predef type: '%s/%d'" line name (List.length args))

let rec of_erl_type = function
  | Erl_type.Any -> TyAny
  | None -> TyBottom
  | Unit ->
     Log.debug [%here] "not implemented conversion from erl_type: Unit";
     of_elem (TySingleton (Atom "not_implemented"))
  | erl_type -> TyUnion (union_of_erl_type erl_type)
and union_of_erl_type = function
  | Erl_type.None -> []
  | AnyAtom -> [TyAtom]
  | AtomUnion atoms -> List.map ~f:(fun atom -> TySingleton (Constant.Atom atom)) atoms
  | Function {params; ret} -> [TyFun (params |> List.map ~f:of_erl_type, of_erl_type ret)]
  | Number (Erl_type.IntSet [n]) -> [TySingleton (Number n)]
  | Number _ -> [TyNumber] (* TODO IntSet with multiple integers *)
  | Var (VAtom var_id) -> [TyVar (Type_variable.of_string var_id)]
  | Tuple {types; _} -> [TyTuple (List.map ~f:of_erl_type types)]
  | NTuplesUnion n_tuples_list ->
      Erl_type.(List.concat_map ~f:(fun {tuples; _} -> List.map ~f:(fun {types; _} -> TyTuple (List.map ~f:of_erl_type types)) tuples) n_tuples_list)
  | Union erl_types -> List.concat_map ~f:union_of_erl_type erl_types
  | Var (VInt _)
  | Binary _
  | AnyIdentifier
  | IdentifierUnion _
  | List _
  | Nil
  | AnyMap
  | Map _
  | OpaqueUnion _
  | AnyTuple
    as other ->
     Log.debug [%here] "not implemented conversion from erl_type: %s" (Erl_type.sexp_of_t other |> Sexp.to_string_hum);
     [TySingleton (Atom "not_implemented")]
  | Any | Unit ->
     failwith "cannot reach here"
