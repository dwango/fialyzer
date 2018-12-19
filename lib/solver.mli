open Type

type solution
[@@deriving sexp_of]

val string_of_sol : solution -> string

val init : solution

val solve : solution -> constraint_ -> (solution, exn) result

(* TODO: don't export *)
val meet : solution -> typ -> typ -> typ
