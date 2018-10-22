open Base
open Ast_intf

type type_var = string

type t =
  (type_var, typ, String.comparator_witness) Map.t

let sexp_of_t : t -> Sexp.t = Map.sexp_of_m__t (module String) sexp_of_typ

let empty : t = Map.empty (module String)
let find = Map.find
let add key data = Map.add_exn ~key ~data
