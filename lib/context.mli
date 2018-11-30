open Base
open Ast_intf

module Key : sig
  type t = Var of String.t | MFA of String.t * String.t * Int.t
  [@@deriving sexp_of, ord]
end

type t

val empty : t
val find : t -> Key.t -> typ option
val add : Key.t -> typ -> t -> t
