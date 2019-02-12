open Base
module Format = Caml.Format

module Key : sig
  type t = Var of String.t | MFA of Mfa.t | LocalFun of {function_name:string; arity: int}
  [@@deriving show, sexp_of]
end

type t

val empty : t
val find : t -> Key.t -> Type.t option
val add : Key.t -> Type.t -> t -> t
val create : import_modules : string list -> t
