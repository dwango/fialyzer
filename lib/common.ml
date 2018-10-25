open Base
let result_map_m rs ~f:f = Result.all (List.map rs ~f:f)

let (!%) = Printf.sprintf
let (<<<) = Fn.compose
let (>>>) f g = g <<< f
