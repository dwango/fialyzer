type type_error = {filename: string; line: int; actual : Type.t; expected: Type.t; message: string}
[@@deriving sexp_of]

type t =
  | InvalidUsage
  | NoSuchFile of string
  | InvalidBeam of {beam_filename: string; message: string}
  | UnboundVariable of {filename: string; line: int; variable: Context.Key.t}
  | TypeError of type_error list
  | TypeSpecUnmatch of {filename: string; line: int; module_name: string; function_name: string;
                        type_spec: Type.t; success_type: Type.t}
  | NotImplemented of {issue_links: string list; message: string}
[@@deriving sexp_of]

exception FialyzerError of t
[@@deriving sexp_of]

val to_message : t -> string
