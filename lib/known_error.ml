open Base
open Common

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

let to_message = function
  | InvalidUsage ->
     "Usage: fialyzer <beam_filename>"
  | NoSuchFile filename ->
     !%"No such beam file '%s'" filename
  | InvalidBeam {beam_filename; message} ->
     !%"Invalid beam format '%s': %s" beam_filename message
  | UnboundVariable {filename; line; variable = Var v; } ->
     !%"%s:%d: Unbound variable: %s" filename line v
  | UnboundVariable {filename; line; variable = LocalFun {function_name; arity}; } ->
     !%"%s:%d: Unbound function: %s/%d" filename line function_name arity
  | UnboundVariable {filename; line; variable = MFA mfa; } ->
     !%"%s:%d: Unknown function: %s" filename line (Mfa.show mfa)
  | TypeError errs ->
     let f err =
     !%"%s:%d: Type error: type mismatch;\n  found   : %s\n  required: %s\n%s" err.filename err.line
       (Type.pp err.actual)
       (Type.pp err.expected)
       err.message
     in
     List.map ~f errs |> String.concat ~sep:"\n"
  | NotImplemented {issue_links; message} ->
     let links = List.map ~f:(!%"- %s") issue_links |> String.concat ~sep:"\n" in
     !%"Not implemented: %s: See the issues:\n%s" message links
