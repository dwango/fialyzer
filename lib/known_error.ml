open Base
module Format = Caml.Format
open Common

type t =
  | InvalidUsage
  | NoSuchFile of string
  | InvalidBeam of {beam_filename: string; message: string}
  | UnboundVariable of {filename: string; line: int; variable: Context.Key.t}
  | TypeError of {filename: string; line: int; actual : Ast_intf.typ; expected: Ast_intf.typ}
  | NotImplemented of {issue_link: string}
[@@deriving show, sexp_of]

exception FialyzerError of t

let to_message = function
  | InvalidUsage ->
     "Usage: fialyzer <beam_filename>"
  | NoSuchFile filename ->
     !%"No such beam file '%s'" filename
  | InvalidBeam {beam_filename; message} ->
     !%"Invalid beam format '%s': %s" beam_filename message
  | UnboundVariable {filename; line; variable = Var v; } ->
     !%"%s:%d: Unbound variable: %s" filename line v
  | UnboundVariable {filename; line; variable = MFA mfa; } ->
     !%"%s:%d: Unknown function: %s" filename line (Mfa.show mfa)
  | TypeError err ->
     !%"%s:%d: Type error: type mismatch;\n  found   : %s\n  required: %s" err.filename err.line
       (Ast_intf.show_typ err.actual)
       (Ast_intf.show_typ err.expected)
  | NotImplemented {issue_link} ->
     !%"Not implemented: Please +1 %s" issue_link
