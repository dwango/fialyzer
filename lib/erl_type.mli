open Base
module Format = Caml.Format
module Etf = Obeam.External_term_format

type qualifier
[@@deriving show, sexp_of]

type ident_type = IPort | IPid | IReference
[@@deriving show, sexp_of]

type var_id =
  | VAtom of string
  | VInt of int
[@@deriving show, sexp_of]

type number =
  | IntRange of {min: min; max: max}
  | IntSet of int list
  | AnyInteger (* integer() *)
  | AnyFloat   (* float()   *)
  | AnyNumber     (* number()  *)
and min
and max
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
  | Unit (* no_return *)
  | AnyAtom
  | AtomUnion of string list
  | Binary of {unit: int; base:int}
  | Function of {params: t list; ret: t}
  | AnyIdentifier
  | IdentifierUnion of ident_type list
  | List of {elem_type: t; terminal_type: t; is_nonempty: bool}
  | Nil
  | Number of number
  | AnyMap
  | Map of {map_pairs: map_pair list; dict: key_value_pair} (* Map can be used as a map or a dictionary, or both *)
  | OpaqueUnion of opaque list
  | Var of var_id
  | AnyTuple
  | Tuple of tuple
  | NTuplesUnion of n_tuples list
  (* | Matchstate of p * slots : TODO *)
  | Union of t list
and map_pair =
  | MandatoryPair of key_value_pair
  | OptionalPair of key_value_pair
and key_value_pair = {key: t; value: t}
and opaque = {
    mod_ : string;
    name : string;
    args : t list;
    struct_ : t;
  }
and tuple = {
    types : t list;      (* type of elements *)
    arity : int;         (* size *)
    tag : string option; (* first element's tag (may be record name) *)
  }
and n_tuples = {
  n: int;
  tuples: tuple list
}
[@@deriving show, sexp_of]

val of_etf : Etf.t -> (t, exn) Result.t
