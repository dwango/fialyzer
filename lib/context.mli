open Base
open Ast_intf
module Format = Caml.Format

module Key : sig
  type t = Var of String.t | MFA of Mfa.t
  [@@deriving show, sexp_of]
end

type t

val empty : t
val find : t -> Key.t -> typ option
val add : Key.t -> typ -> t -> t
val init : unit -> t
