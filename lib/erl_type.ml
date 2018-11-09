open Base
module Format = Caml.Format
module Etf = Obeam.External_term_format
module E = Etf_util
open Common

type qualifier = FloatQual | IntegerQual | NonemptyQual | PidQual | PortQual
                 | ReferenceQual | UnknownQual
                 (* | OtherQual of etf * etf *)
[@@deriving show, sexp_of]

let qualifier_of_etf etf =
  let open Result in
  let qual_of_atom = function
    | "float" -> Ok FloatQual
    | "integer" -> Ok IntegerQual
    | "nonempty" -> Ok NonemptyQual
    | "pid" -> Ok PidQual
    | "port" -> Ok PortQual
    | "reference" -> Ok ReferenceQual
    | "unknown" -> Ok UnknownQual
    | other -> Error (Failure(!%"qual_of_atom: unknown : %s" other))
  in
  E.atom_of_etf etf >>= fun atom ->
  qual_of_atom atom

type t_map_mandatoriness = Mandatory | Optional
[@@deriving show, sexp_of]

let mandatoriness_of_etf = function
  | Etf.Atom "mand" -> Ok Mandatory
  | Atom "opt" -> Ok Optional
  | _ -> Error (Failure "mandatoriness_of_etf")

type t =
  | Any
  | None
  | Unit
  | Atom of string list
  (*  | Bitstr of : TODO *)
  | Function of t list * t
  (* | Identifier of : TODO *)
  | List of t * t * qualifier (* (types, term, size): TODO *)
  | Nil
  (*  | Number of set * qualifier : TODO *)
  | Map of t_map_pair list * t * t (* (t_map_dict, defkey, defval) : TODO *)
  | Opaque of opaque list
  | Product of t list
  | Tuple of t list * int * t (* (types, arity, tag) : TODO *)
  (* | TupleSet of tuples : TODO *)
  (* | Var of id : TODO *)
 (* | Matchstate of p * slots : TODO *)
  | Union of t list
[@@deriving show, sexp_of]
and t_map_pair = t * t_map_mandatoriness * t
[@@deriving show, sexp_of]
and opaque = {
    mod_ : string;
    name : string;
    args : t list;
    struct_ : t;
  }
[@@deriving show, sexp_of]
