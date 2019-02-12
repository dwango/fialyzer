open Base
open Common

let debug_mode = ref false

let write s =
  if !debug_mode then
    Caml.prerr_endline s
  else
    ()

let string_of_time time =
  let tm = Unix.localtime time in
  Unix.(!%"%04d-%02d-%02dT%02d:%02d:%02d" (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
          tm.tm_hour tm.tm_min tm.tm_sec)

let make_message position text =
  let now = Unix.time () in
  !%"%s (%s) %s" (string_of_time now) (Source_code_position.to_string position) text

let debug position f =
  Caml.Printf.kprintf (fun s -> make_message position s |> write) f
