open Base

val derive : Context.t -> Ast.t -> (Type.t * Type.constraint_, exn) Result.t
