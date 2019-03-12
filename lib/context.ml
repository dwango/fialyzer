open Base
open Poly
module Format = Caml.Format

module Key = struct
  type t = Var of String.t | MFA of Mfa.t | LocalFun of {function_name:string; arity: int}
  [@@deriving show, sexp_of]
end

module MapOnKey = Poly_map.Make(Key)

(*type t = typ Map.M(Key).t*)
type t = Type.t MapOnKey.t

let empty : t = MapOnKey.empty
let find = Map.find
let add key data t =
  match Map.add ~key ~data t with
  | `Ok t' -> t'
  | `Duplicate ->
     Log.debug [%here] "variable '%s' shadowed" (Key.show key);
     t

let update imports (mfa, ty) ctx =
  if List.mem imports mfa.Mfa.module_name ~equal:(=) then
    add (Key.MFA mfa) ty ctx
    |> add (Key.LocalFun {function_name=mfa.Mfa.function_name; arity=mfa.Mfa.arity}) ty
  else
    add (Key.MFA mfa) ty ctx

let add_bif_signatures imports ctx0 : t =
  Bif.type_sigs
  |> List.fold_left ~f:(fun ctx (mfa,ty) -> update imports (mfa,ty) ctx) ~init:ctx0

let type_of_contract (cont: Plt.contract) =
  let subst_map map ty =
    Map.fold map ~init:ty ~f:(fun ~key:v ~data:ty0 ty -> Type.subst (v, ty0) ty)
  in
  let type_var_of_var_id = function
    | Erl_type.VAtom v -> Type_variable.of_string v
    | Erl_type.VInt i -> failwith "TODO: type_var_of_var_id: VInt"
  in
  let rec assoc map = function
    | [] -> map
    | (v, ty) :: pairs ->
       let map' = Map.map ~f:(Type.subst (v, ty)) map |> Map.add_exn ~key:v ~data:ty in
       assoc map' (List.Assoc.map ~f:(subst_map map') pairs)
  in
  let f c =
    let fun_ty = c.Plt.f |> Type.of_erl_type in
    let map =
      c.Plt.constraints
      |> List.map ~f:(fun Plt.(Subtyping {lhs=v; rhs=ty}) -> (type_var_of_var_id v, ty))
      |> List.Assoc.map ~f:Type.of_erl_type
      |> assoc (Map.empty (module Type_variable))
    in
    subst_map map fun_ty
  in
  cont.Plt.contracts
  |> List.map ~f
  |> Type.union_list

let add_plt_signatures imports plt ctx =
  plt.Plt.contracts
  |> Map.map ~f:type_of_contract
  |> Map.fold ~f:(fun ~key:mfa ~data:ty ctx ->
                update imports (mfa,ty) ctx
              )
              ~init:ctx

let create ~import_modules plt_opt =
  empty
  |> add_bif_signatures import_modules
  |> match plt_opt with
     | Some plt -> add_plt_signatures import_modules plt
     | None -> (fun ctx -> ctx)
