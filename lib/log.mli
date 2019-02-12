open Base

val debug_mode : bool ref
val debug : Lexing.position -> ('a, unit, string, unit) format4 -> 'a
