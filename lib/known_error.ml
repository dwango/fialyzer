open Base
module Format = Caml.Format
open Common

type type_error = {
    filename : string;
    line: int;
    actual : Ast_intf.typ;
    expected: Ast_intf.typ;
  }
[@@deriving show, sexp_of]

type issue = {
    url : string;
  }
[@@deriving show, sexp_of]

type t =
  | InvalidUsage
  | NoSuchFile of string
  | InvalidBeam of {beam_filename: string; message: string}
  | UnboundVariable of {filename: string; line: int; variable: Context.Key.t}
  | TypeError of type_error
  | NotImplemented of issue
[@@deriving show, sexp_of]

exception FialyzerError of t

let make_type_error ~filename ~line ~actual ~expected =
  {filename; line; actual; expected}

let make_issue ~url = {url}

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
  | NotImplemented issue ->
     !%"Not implemented: see the issue %s" issue.url
