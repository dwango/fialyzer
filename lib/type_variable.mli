type t
[@@deriving show, sexp_of]

type comparator_witness

val comparator : (t, comparator_witness) Base.Comparator.t
val create : unit -> t
val reset_count : unit -> unit

val of_string : string -> t
val to_string : t -> string
