open Base
module Format = Caml.Format
module Etf = Obeam.External_term_format
open Common

type etf = Etf.t
let sexp_of_etf etf = sexp_of_string (Etf.show etf)

let atom_of_etf = function
  | Etf.Atom atom -> Ok atom
  | other -> Error(Failure (!%"atom_of_etf: %s" (Etf.show other)))

let tuple_of_etf = function
  | Etf.SmallTuple(_, etfs) -> Ok etfs
  | other -> Error(Failure (!%"tuple_of_etf: %s" (Etf.show other)))

let pair_of_etf etf =
  let open Result in
  tuple_of_etf etf >>= function
  | [x; y] -> Ok (x, y)
  | other ->
     Error (Failure (!%"pair_of_etf: [%s]" (List.map ~f:Etf.show other |> String.concat ~sep:",")))

let list_of_etf = function
  | Etf.Nil -> Ok []
  | List(bkt, Nil) -> Ok bkt
  | other ->
     Error (Failure(!%"list_of_etf: '%s'" (Etf.show other)))

let int_of_etf = function
  | Etf.SmallInteger i -> Ok i
  | other -> Error (Failure (!%"int_of_etf: %s" (Etf.show other)))

(* ==========================================================================
   Basic erlang data structure: set
   ========================================================================== *)

type set = {
    set_size : int;
    set_num_of_active_slot : int;
    set_maxn : int;
    set_buddy_slot_offset : int;
    set_exp_size : int;
    set_con_size : int;
    set_empty_segment : Etf.t;
    set_segs : Etf.t list;
  }
[@@deriving show]

let set_of_etf = function
  | Etf.SmallTuple(9, [
        Atom "set";
        SmallInteger set_size;
        SmallInteger set_num_of_active_slot;
        SmallInteger set_maxn;
        SmallInteger set_buddy_slot_offset;
        SmallInteger set_exp_size;
        SmallInteger set_con_size;
        set_empty_segment;
        SmallTuple (_, set_segs);
    ]) ->
     Ok {set_size; set_num_of_active_slot; set_maxn; set_buddy_slot_offset;
         set_exp_size; set_con_size; set_empty_segment; set_segs}
  | other ->
     Error (Failure (!%"set_of_etf: %s" (Etf.show other)))

let fold_elist ~f ~init etf =
  match list_of_etf etf with
  | Ok etfs -> List.fold_left ~f ~init etfs
  | Error exn -> raise exn
let fold_seg ~f ~init seg =
  let[@warning "-8"] Etf.SmallTuple (_, bs) = seg in
  List.fold_left ~f:(fun acc elist -> fold_elist ~f ~init:acc elist) ~init bs
let fold_segs ~f ~init segs =
  List.fold_left ~f:(fun acc seg -> fold_seg ~f ~init:acc seg) ~init segs
let fold_set ~f ~init set =
  fold_segs ~f ~init set.set_segs
let to_list set =
  fold_segs ~f:(fun xs x -> x :: xs) ~init:[] set.set_segs

(* ==========================================================================
   Basic erlang data structure: dict
   ========================================================================== *)

type dict = {
    dict_size : int;
    dict_num_of_active_slot : int;
    dict_maxn : int;
    dict_buddy_slot_offset : int;
    dict_exp_size : int;
    dict_con_size : int;
    dict_empty_segment : Etf.t;
    dict_segs : Etf.t list;
  }
[@@deriving show]

let dict_of_etf = function
  | Etf.SmallTuple(9, [
        Atom "dict";
        SmallInteger dict_size;
        SmallInteger dict_num_of_active_slot;
        SmallInteger dict_maxn;
        SmallInteger dict_buddy_slot_offset;
        SmallInteger dict_exp_size;
        SmallInteger dict_con_size;
        dict_empty_segment;
        SmallTuple (_, dict_segs);
    ]) ->
     Ok {dict_size; dict_num_of_active_slot; dict_maxn; dict_buddy_slot_offset;
         dict_exp_size; dict_con_size; dict_empty_segment; dict_segs}
  | other ->
     Error (Failure (!%"dict_of_etf"))

let fold_dict ~f ~init dict =
  let segs = dict.dict_segs in
  fold_segs ~f:(fun acc e ->
      match e with
      | List([k; v], Nil) -> f acc k v
      | List([k], v) -> f acc k v
      | other -> failwith (!%"fold_dict: %s" (Etf.show other))
    ) ~init segs
let dict_to_list dict =
  fold_dict ~f:(fun acc k v -> (k, v) :: acc) ~init:[] dict
