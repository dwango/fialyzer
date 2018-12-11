open Base
open Ast_intf
module Format = Caml.Format

module Key = struct
  type t = Var of String.t | MFA of Mfa.t
  [@@deriving show, sexp_of]
end

module MapOnKey = Poly_map.Make(Key)

(*type t = typ Map.M(Key).t*)
type t = typ MapOnKey.t

let empty : t = MapOnKey.empty
let find = Map.find
let add key data = Map.add_exn ~key ~data

let add_bif_signatures ctx0 : t =
  Bif.type_sigs
  |> List.fold_left ~f:(fun ctx (mfa, ty) ->
                      add (Key.MFA mfa) ty ctx) ~init:ctx0

let init () =
  empty
  |> add_bif_signatures
