open Base
open Ast_intf

module TypeVar = String

type t = typ Map.M(TypeVar).t

let empty : t = Map.empty (module String)
let find = Map.find
let add key data = Map.add_exn ~key ~data
