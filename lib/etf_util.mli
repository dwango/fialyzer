open Base
module Format = Caml.Format
module Etf = Obeam.External_term_format

type etf = Etf.t
val show_etf : Etf.t -> string
val sexp_of_etf : etf -> Sexplib0.Sexp.t

val atom_of_etf : Etf.t -> (string, exn) Result.t
val tuple_of_etf : Etf.t -> (Etf.t list, exn) Result.t
val pair_of_etf : Etf.t -> ((Etf.t * Etf.t) , exn) Result.t
val list_of_etf : Etf.t -> (Etf.t list, exn) Result.t
val int_of_etf : Etf.t -> (int, exn) Result.t

val list : Etf.t list -> Etf.t
val small_tuple : Etf.t list -> Etf.t
val pair : Etf.t -> Etf.t -> Etf.t

(** =========================================================================
    Basic erlang data structure: set
    ========================================================================= *)

(** Erlang [set()] type
    @see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/stdlib/src/sets.erl#L62-L72>
 *)
type set
[@@deriving show]

val set_of_etf : Etf.t -> (set, exn) Result.t
val fold_set : f:('acc -> Etf.t -> 'acc) -> init:'acc -> set -> 'acc
val to_list : set -> Etf.t list
val empty_set : set
val etf_of_set : set -> Etf.t

(** =========================================================================
    Basic erlang data structure: dict
    ========================================================================= *)

(** erlang dict() type
    @see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/stdlib/src/dict.erl#L61-L71>
 *)
type dict
[@@deriving show]

val dict_of_etf : Etf.t -> (dict, exn) Result.t
val fold_dict : f:('acc -> Etf.t -> Etf.t -> 'acc) -> init:'acc -> dict -> 'acc
val dict_to_list : dict -> (Etf.t * Etf.t) list
val empty_dict : dict
val etf_of_dict : dict -> Etf.t
