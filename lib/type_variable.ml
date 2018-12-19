let sexp_of_string = Base.sexp_of_string

type t = string
[@@deriving show, sexp_of]

type comparator_witness = Base.String.comparator_witness

let comparator = Base.String.comparator

let count = ref (-1)

let create () =
  incr count;
  if !count <= (int_of_char 'z' - int_of_char 'a') then
    Printf.sprintf "%c" (char_of_int (int_of_char 'a' + !count))
  else
    Printf.sprintf "%s%02d" "v" !count

let reset_count () =
  count := -1

let of_string s = s

let to_string s = s
