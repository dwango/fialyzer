type t =
  | Eq of {lhs: Type.t; rhs: Type.t; expr: Ast.t}
  | Subtype of {lhs: Type.t; rhs: Type.t; expr: Ast.t}
  | Conj of t list
  | Disj of t list
  | Empty
[@@deriving sexp_of]

val show : t -> string
