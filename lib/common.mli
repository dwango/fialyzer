open Base
val result_map_m : 'a list -> f:('a -> ('b, 'c) Result.t) -> ('b list, 'c) Result.t

