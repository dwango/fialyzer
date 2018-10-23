open Base
let result_map_m rs ~f:f = Result.all (List.map rs ~f:f)
