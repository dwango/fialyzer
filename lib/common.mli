open Base

val result_map_m : 'a list -> f:('a -> ('b, 'c) Result.t) -> ('b list, 'c) Result.t
val result_guard : ?error:exn -> bool -> (unit, exn) Result.t
val result_or : ('a, 'e) Result.t -> ('a, 'e) Result.t -> ('a, 'e) Result.t
val (@?) : ('a, exn) Result.t -> string -> ('a, exn) Result.t

val (!%) : ('a, unit, string) format -> 'a
val (<<<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val (>>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val map_add_if_not_exists : 'a -> 'b -> ('a, 'b, 'c) Map.t -> ('a, 'b, 'c) Map.t

val list_of_option : 'a option -> 'a list

val list_group_by : f:('a -> 'b) -> 'a list -> ('b * 'a list) list
