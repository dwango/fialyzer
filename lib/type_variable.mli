type t
[@@deriving show, sexp_of]

type comparator_witness

val create : unit -> t
val reset_count : unit -> unit
