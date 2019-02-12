open Base
module Format = Caml.Format
module Etf = Obeam.External_term_format
open Common

type etf = Etf.t

let rec show_etf = function
  | Etf.SmallInteger i -> !%"%d"i
  | Integer i32 -> !%"%ld" i32
  | Float s -> s
  | Atom atom -> !%"'%s'" atom
  | SmallTuple (_n, ts) ->
     List.map ~f:show_etf ts
     |> String.concat ~sep:", "
     |> !%"{%s}"
  | Map (_i32, kvs) ->
     List.map ~f:(fun (k,v) -> !%"%s => %s" (show_etf k) (show_etf v)) kvs
     |> String.concat ~sep:", "
     |> !%"#{%s}"
  | Nil -> "[]"
  | String s -> !%"\"%s\"" (String.escaped s)
  | Binary _bitstr -> "<<...>>"
  | SmallBig z -> Z.to_string z
  | LargeBig z -> Z.to_string z
  | List (ts, Nil) ->
     List.map ~f:show_etf ts
     |> String.concat ~sep:", "
     |> !%"[%s]"
  | List (ts, termination) ->
     let lst = List.map ~f:show_etf ts |> String.concat ~sep:", " in
     !%"[%s | %s]" lst (show_etf termination)
  | NewFloat f -> !%"%f" f
  | AtomUtf8 s -> !%"'%s'" s
  | SmallAtomUtf8 s ->
     !%"'%s'" s

let sexp_of_etf etf = sexp_of_string (show_etf etf)

let atom_of_etf = function
  | Etf.Atom atom -> Ok atom
  | other -> Error(Failure (!%"atom_of_etf: %s" (show_etf other)))

let tuple_of_etf = function
  | Etf.SmallTuple(_, etfs) -> Ok etfs
  | other -> Error(Failure (!%"tuple_of_etf: %s" (show_etf other)))

let pair_of_etf etf =
  let open Result in
  tuple_of_etf etf >>= function
  | [x; y] -> Ok (x, y)
  | other ->
     Error (Failure (!%"pair_of_etf: [%s]" (List.map ~f:show_etf other |> String.concat ~sep:",")))

let list_of_etf = function
  | Etf.Nil -> Ok []
  | List(bkt, Nil) -> Ok bkt
  | String s -> (* an optimization by ETF: see http://erlang.org/doc/apps/erts/erl_ext_dist.html#string_ext *)
     String.to_list s
     |> List.map ~f:(fun char -> Etf.SmallInteger (Char.to_int char))
     |> Result.return
  | other ->
     Error (Failure(!%"list_of_etf: '%s'" (show_etf other)))

let int_of_etf = function
  | Etf.SmallInteger i -> Ok i
  | Etf.Integer i32 -> Ok (Int32.to_int_exn i32)
  | other -> Error (Failure (!%"int_of_etf: %s" (show_etf other)))

let list etfs = Etf.List(etfs, Etf.Nil)
let small_tuple etfs = Etf.SmallTuple(List.length etfs, etfs)
let pair etf1 etf2 = small_tuple [etf1; etf2]

let etf_printer fmt etf = show_etf etf |> Format.fprintf fmt "%s"
let etfs_printer fmt etfs = List.map ~f:show_etf etfs |> String.concat ~sep:"\n" |> Format.fprintf fmt "%s"

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
    set_empty_segment : Etf.t [@printer etf_printer];
    set_segs : Etf.t list [@printer etfs_printer];
  }
[@@deriving show]

let set_of_etf etf =
  let open Result in
  tuple_of_etf etf >>= function
  | [Atom "set";
     set_size_etf;
     set_num_of_active_slot_etf;
     set_maxn_etf;
     set_buddy_slot_offset_etf;
     set_exp_size_etf;
     set_con_size_etf;
     set_empty_segment;
     set_segs_etf;
    ] ->
     tuple_of_etf set_segs_etf >>= fun set_segs ->
     int_of_etf set_size_etf >>= fun set_size ->
     int_of_etf set_num_of_active_slot_etf >>= fun set_num_of_active_slot ->
     int_of_etf set_maxn_etf >>= fun set_maxn ->
     int_of_etf set_buddy_slot_offset_etf >>= fun set_buddy_slot_offset ->
     int_of_etf set_exp_size_etf >>= fun set_exp_size ->
     int_of_etf set_con_size_etf >>= fun set_con_size ->
     Ok {set_size; set_num_of_active_slot; set_maxn; set_buddy_slot_offset;
         set_exp_size; set_con_size; set_empty_segment; set_segs}
  | _ ->
     Error (Failure (!%"set_of_etf: %s" (show_etf etf)))

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

let empty_set =
  let seg_size = 16 in
  let expand_load = 5 in
  let contract_load = 3 in
  let exp_size = seg_size * expand_load in
  let con_size = seg_size * contract_load in
  let empty_segs =
    small_tuple Etf.[Nil; Nil; Nil; Nil;  Nil; Nil; Nil; Nil;
                     Nil; Nil; Nil; Nil;  Nil; Nil; Nil; Nil;]
  in
  {
    set_size = 0;
    set_num_of_active_slot = seg_size;
    set_maxn = seg_size;
    set_buddy_slot_offset = seg_size / 2;
    set_exp_size = exp_size;
    set_con_size = con_size;
    set_empty_segment = empty_segs;
    set_segs = [empty_segs];
  }

let etf_of_set set =
  Etf.(small_tuple [
           Atom "set";
           SmallInteger set.set_size;
           SmallInteger set.set_num_of_active_slot;
           SmallInteger set.set_maxn;
           SmallInteger set.set_buddy_slot_offset;
           SmallInteger set.set_exp_size;
           SmallInteger set.set_con_size;
           set.set_empty_segment;
           small_tuple set.set_segs;
  ])

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
    dict_empty_segment : Etf.t [@printer etf_printer];
    dict_segs : Etf.t list [@printer etfs_printer];
  }
[@@deriving show]

let dict_of_etf etf =
  let open Result in
  tuple_of_etf etf >>= function
  | [Atom "dict";
     dict_size_etf;
     dict_num_of_active_slot_etf;
     dict_maxn_etf;
     dict_buddy_slot_offset_etf;
     dict_exp_size_etf;
     dict_con_size_etf;
     dict_empty_segment;
     dict_segs_etf;
    ] ->
     tuple_of_etf dict_segs_etf >>= fun dict_segs ->
     int_of_etf dict_size_etf >>= fun dict_size ->
     int_of_etf dict_num_of_active_slot_etf >>= fun dict_num_of_active_slot ->
     int_of_etf dict_maxn_etf >>= fun dict_maxn ->
     int_of_etf dict_buddy_slot_offset_etf >>= fun dict_buddy_slot_offset ->
     int_of_etf dict_exp_size_etf >>= fun dict_exp_size ->
     int_of_etf dict_con_size_etf >>= fun dict_con_size ->
     Ok {dict_size; dict_num_of_active_slot; dict_maxn; dict_buddy_slot_offset;
         dict_exp_size; dict_con_size; dict_empty_segment; dict_segs}
  | other ->
     Error (Failure (!%"dict_of_etf: %s" (show_etf etf)))

let fold_dict ~f ~init dict =
  let segs = dict.dict_segs in
  fold_segs ~f:(fun acc e ->
      match e with
      | List([k; v], Nil) -> f acc k v
      | List([k], v) -> f acc k v
      | other -> failwith (!%"fold_dict: %s" (show_etf other))
    ) ~init segs
let dict_to_list dict =
  fold_dict ~f:(fun acc k v -> (k, v) :: acc) ~init:[] dict

let empty_dict =
  let seg_size = 16 in
  let expand_load = 5 in
  let contract_load = 3 in
  let exp_size = seg_size * expand_load in
  let con_size = seg_size * contract_load in
  let empty_segs =
    small_tuple Etf.[Nil; Nil; Nil; Nil;  Nil; Nil; Nil; Nil;
                     Nil; Nil; Nil; Nil;  Nil; Nil; Nil; Nil;]
  in
  {
    dict_size = 0;
    dict_num_of_active_slot = seg_size;
    dict_maxn = seg_size;
    dict_buddy_slot_offset = seg_size / 2;
    dict_exp_size = exp_size;
    dict_con_size = con_size;
    dict_empty_segment = empty_segs;
    dict_segs = [empty_segs];
  }

let etf_of_dict dict =
  Etf.(small_tuple [
           Atom "dict";
           SmallInteger dict.dict_size;
           SmallInteger dict.dict_num_of_active_slot;
           SmallInteger dict.dict_maxn;
           SmallInteger dict.dict_buddy_slot_offset;
           SmallInteger dict.dict_exp_size;
           SmallInteger dict.dict_con_size;
           dict.dict_empty_segment;
           small_tuple dict.dict_segs;
  ])
