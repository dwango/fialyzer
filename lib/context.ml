open Base
open Polymorphic_compare
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

let add_bif_signatures imports ctx0 : t =
  let update ctx (mfa, ty) =
    if List.mem imports mfa.Mfa.module_name ~equal:(=) then
      add (Key.MFA mfa) ty ctx
      |> add (Key.LocalFun {function_name=mfa.Mfa.function_name; arity=mfa.Mfa.arity}) ty
    else
      add (Key.MFA mfa) ty ctx
  in
  Bif.type_sigs
  |> List.fold_left ~f:update ~init:ctx0

let create ~import_modules =
  empty
  |> add_bif_signatures import_modules
