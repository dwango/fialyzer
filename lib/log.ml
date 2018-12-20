open Base
open Common
open Polymorphic_compare

let log_file =
  let tm = Unix.(localtime (time ())) in
  Unix.(!%"./fialyzer_log.%04d%02d%02d_%02d%2d" (1900 + tm.tm_year) (1 + tm.tm_mon)
          tm.tm_mday tm.tm_hour tm.tm_min)

let open_file filename =
  if Caml.Sys.file_exists filename = false then
    Caml.open_out filename
  else
    Caml.open_out_gen [Open_wronly; Open_append; Open_text] 0o666 filename

let open_file_with filename f =
  let ch = open_file filename in
  let r = Result.try_with (fun () -> f ch) in
  Caml.close_out ch;
  Result.ok_exn r

let write s =
  open_file_with log_file (fun ch ->
                   Caml.output_string ch s; Caml.flush ch)

let string_of_time time =
  let tm = Unix.localtime time in
  Unix.(!%"%04d-%02d-%02dT%02d:%02d:%02d" (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
          tm.tm_hour tm.tm_min tm.tm_sec)

let make_message position text =
  let now = Unix.time () in
  !%"%s (%s) %s\n" (string_of_time now) (Source_code_position.to_string position) text

let debug position f =
  Caml.Printf.kprintf (fun s -> make_message position s |> write) f
