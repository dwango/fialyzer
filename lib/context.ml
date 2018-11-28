open Base
open Ast_intf

module Key = struct
  module T = struct
    type t = Var of String.t | MFA of String.t * String.t * Int.t
    [@@deriving sexp_of, ord]
  end
  include T
  include Comparator.Make(T)
end

type t = typ Map.M(Key).t

let empty : t = Map.empty (module Key)
let find = Map.find
let add key data = Map.add_exn ~key ~data
