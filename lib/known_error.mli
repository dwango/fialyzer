type type_error
[@@deriving show, sexp_of]

type issue
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

val make_type_error : filename:string -> line:int -> actual:Ast_intf.typ -> expected:Ast_intf.typ -> type_error
val make_issue : url:string -> issue
val to_message : t -> string
