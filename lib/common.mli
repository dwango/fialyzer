open Base

val result_map_m : 'a list -> f:('a -> ('b, 'c) Result.t) -> ('b list, 'c) Result.t
val (!%) : ('a, unit, string) format -> 'a
val (<<<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val (>>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
