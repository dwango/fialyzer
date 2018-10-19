open Base
open Ast_intf

type type_var = string

type t =
  (type_var, typ, String.comparator_witness) Map.t

let empty : t = Map.empty (module String)
let find = Map.find
let add key data = Map.add_exn ~key ~data
