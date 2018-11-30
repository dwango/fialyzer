open Base
module Format = Caml.Format
module Etf = Obeam.External_term_format

type qualifier
[@@deriving show, sexp_of]

type t_map_mandatoriness
[@@deriving show, sexp_of]

type ident_type
[@@deriving show, sexp_of]

type var_id
[@@deriving show, sexp_of]

(**
{v
-record(c, {tag			      :: tag(),
	    elements  = []	      :: term(),
	    qualifier = ?unknown_qual :: qual()}).

-opaque erl_type() :: ?any | ?none | ?unit | #c{}.
v}
@see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/hipe/cerl/erl_types.erl>
 *)
type t =
  | Any
  | None
  | Unit
  | Atom of string list
  (*  | Bitstr of : TODO *)
  | Function of t list * t
  | Identifier of ident_type list
  | List of t * t * qualifier
  | Nil
  | Number of qualifier
  | Map of t_map_pair list * t * t
  | Opaque of opaque list
  | Product of t list
  | Var of var_id
  | Tuple of tuple option
  | TupleSet of (int * tuple list) list
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
and tuple
[@@deriving show, sexp_of]

val of_etf : Etf.t -> (t, exn) Result.t
