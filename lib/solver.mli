type solution
[@@deriving sexp_of]

val string_of_sol : solution -> string

val init : solution

val solve : solution -> Type.constraint_ -> (solution, exn) result

(* TODO: don't export *)
val meet : Type.t -> Type.t -> Type.t
