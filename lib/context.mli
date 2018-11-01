open Base
open Ast_intf

module TypeVar = String

type t

val empty : t
val find : t -> TypeVar.t -> typ option
val add : TypeVar.t -> typ -> t -> t
