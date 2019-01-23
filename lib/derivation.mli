open Base

val derive : Context.t -> Ast.t -> (Type.t * Constraint.t, exn) Result.t

(* export for unit-test *)
val pattern_to_expr : Ast.pattern' -> Ast.t
val variables_in_pattern : Ast.pattern' -> (string * Type.t) list
