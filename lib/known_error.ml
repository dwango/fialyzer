open Base
module Format = Caml.Format
open Common

type type_error = {
    file_name : string;
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
  | InvalidBeam of string
  | TypeError of type_error
  | NotImplemented of issue
[@@deriving show, sexp_of]

exception FialyzerError of t

let make_type_error ~file_name ~line ~actual ~expected =
  {file_name; line; actual; expected}

let make_issue ~url = {url}

let to_message = function
  | InvalidUsage ->
     "Usage: fialyzer <beam_filename>"
  | NoSuchFile filename ->
     !%"No such beam file '%s'" filename
  | InvalidBeam filename ->
     !%"Invalid beam format '%s'" filename
  | TypeError err ->
     !%"%s:%d: Type error: type mismatch;\n  found   : %s\n  required: %s" err.file_name err.line
       (Ast_intf.show_typ err.actual)
       (Ast_intf.show_typ err.expected)
  | NotImplemented issue ->
     !%"Not implemented: see the issue %s" issue.url
