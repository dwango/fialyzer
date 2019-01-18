open Base
module Format = Caml.Format
module Etf = Obeam.External_term_format

type qualifier
[@@deriving show, sexp_of]

type t_map_mandatoriness
[@@deriving show, sexp_of]

type ident_type = IAny | IPort | IPid | IReference
[@@deriving show, sexp_of]

type var_id
[@@deriving show, sexp_of]

type number
and int_or_neg_inf
and int_or_pos_inf
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
  | Atom of {atoms_union: string list}
  | Binary of {unit: int; base:int}
  | Function of {params: t list; ret: t}
  | Identifier of {idents_union: ident_type list}
  | List of {elem_type: t; term_type: t; is_nonempty: bool}
  | Nil
  | Number of number
  | Map of t_map_pair list * t * t
  | Opaque of {opaques_union: opaque list}
  | Var of {id: var_id}
  | Tuple of {tuple: tuple_or_any_tuple}
  | TupleSet of {n_tuples_union: n_tuples list}
  (* | Matchstate of p * slots : TODO *)
  | Union of t list
and t_map_pair = t * t_map_mandatoriness * t
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
and tuple_or_any_tuple
and n_tuples
[@@deriving show, sexp_of]

val of_etf : Etf.t -> (t, exn) Result.t
