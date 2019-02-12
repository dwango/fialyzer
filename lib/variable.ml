let count = ref (-1)

let create () =
  incr count;
  if !count <= (int_of_char 'Z' - int_of_char 'A') then
    Printf.sprintf "__%c__" (char_of_int (int_of_char 'A' + !count))
  else
    Printf.sprintf "__%s%02d__" "V" !count

let reset_count () =
  count := -1
