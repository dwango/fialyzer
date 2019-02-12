type type_error = {filename: string; line: int; actual : Type.t; expected: Type.t; message: string}
[@@deriving sexp_of]

type t =
  | InvalidUsage
  | NoSuchFile of string
  | InvalidBeam of {beam_filename: string; message: string}
  | UnboundVariable of {filename: string; line: int; variable: Context.Key.t}
  | TypeError of type_error list
  | NotImplemented of {issue_links: string list; message: string}
[@@deriving sexp_of]

exception FialyzerError of t
[@@deriving sexp_of]

val to_message : t -> string
