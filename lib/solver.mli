type solution = Type.t Base.Map.M(Type_variable).t (* public for test *)

[@@deriving sexp_of]

val string_of_sol : solution -> string

val init : solution

val solve : solution -> Constraint.t -> (solution, exn) result

val lookup_type : solution -> Type.t -> Type.t
