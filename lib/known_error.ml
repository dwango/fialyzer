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
  | InvalidBeam of string * string (* (beam_filename, message) *)
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
  | InvalidBeam (filename, message) ->
     !%"Invalid beam format '%s': %s" filename message
  | TypeError err ->
     !%"%s:%d: Type error: type mismatch;\n  found   : %s\n  required: %s" err.filename err.line
       (Ast_intf.show_typ err.actual)
       (Ast_intf.show_typ err.expected)
  | NotImplemented issue ->
     !%"Not implemented: see the issue %s" issue.url
