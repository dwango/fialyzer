open Base
module Format = Caml.Format

module Key = struct
  type t = Var of String.t | MFA of Mfa.t
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

let add_bif_signatures ctx0 : t =
  Bif.type_sigs
  |> List.fold_left ~f:(fun ctx (mfa, ty) ->
                      add (Key.MFA mfa) ty ctx) ~init:ctx0

let init () =
  empty
  |> add_bif_signatures
