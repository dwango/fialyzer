open Base

val derive : Context.t -> Ast.t -> (Type.t * Constraint.t, exn) Result.t
