open Base

val result_map_m : 'a list -> f:('a -> ('b, 'c) Result.t) -> ('b list, 'c) Result.t
val result_guard : ?error:exn -> bool -> (unit, exn) Result.t
val result_or : ('a, 'e) Result.t -> ('a, 'e) Result.t -> ('a, 'e) Result.t
val (@?) : ('a, exn) Result.t -> string -> ('a, exn) Result.t

val (!%) : ('a, unit, string) format -> 'a
val (<<<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val (>>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
