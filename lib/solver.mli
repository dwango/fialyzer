type solution
[@@deriving sexp_of]

val string_of_sol : solution -> string

val init : solution

val solve : solution -> Type.constraint_ -> (solution, exn) result

val lookup_type : solution -> t -> t
