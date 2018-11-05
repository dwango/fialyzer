open Base
open Obeam
module Etf = External_term_format
open Common
open Ast_intf
module Format = Caml.Format

type mfa = string * string * int
[@@deriving show]

type ret_args_types = typ * typ list
[@@deriving show]

type ('a, 'b) map = ('a * 'b) list
[@@deriving show]

type t = {
    info : (module_, ret_args_types) map;
    types : (mfa, ret_args_types) map;
    contracts : (mfa, ret_args_types) map;
  }

let rec show_etf = function
  | Etf.SmallInteger i -> !%"SInt(%d)" i
  | Integer i32 -> Int32.to_string i32
  | Atom atom -> atom
  | SmallTuple (_arity, elems) ->
     !%"{%s}" (List.map ~f:show_etf elems |> Caml.String.concat ", ")
  | Nil -> "nil"
  | String s -> !%{|"%s"|} s
  | Binary bitstr -> !%{|<<"%s">>|} (Bitstring.string_of_bitstring bitstr)
  | List (elems, tail) ->
     !%"[%s | %s]" (List.map ~f:show_etf elems |> Caml.String.concat ", ")
       (show_etf tail)
  | NewFloat f -> !%"%f" f
  | SmallAtomUtf8 s -> !%"`%s`" s

type infoval = (Etf.t * Etf.t list)
[@@deriving show]
type file_md5 = {
    filename : string;
    binary : string;
  }
[@@deriving show]

type dict = (Etf.t, Etf.t) map
[@@deriving show]

type file_plt = {
    version : string;
    file_md5_list : file_md5 list;
    info : (mfa, infoval) map;
    contracts : (mfa, Etf.t) map;
    callbacks : (Etf.t, Etf.t) map;
    types : (Etf.t, Etf.t) map;
    exported_types : Etf.t list;
    mod_deps : (Etf.t, Etf.t) map;
    implementation_md5 : file_md5 list;
  }
[@@deriving show]

let rec fold_bucket f acc = function
  | Etf.Nil | List ([], Nil) -> acc
  | List(List([k;v], Nil) :: bkt, tl) -> fold_bucket f (f k v acc) (List(bkt,tl))
  | List(List([k],v) :: bkt, tl) -> fold_bucket f (f k v acc) (List(bkt,tl))
  | other ->
     failwith (!%"fold_bucket: '%s'" (show_etf other))
let rec fold_seg f acc seg i =
  if i = 0 then acc
  else
    let[@warning "-8"] Some bkt = List.nth seg (i-1) in
    fold_seg f (fold_bucket f acc bkt) seg (i-1)
let rec fold_segs f acc segs i =
  if i = 0 then acc
  else
    let[@warning "-8"] Some (Etf.SmallTuple (n, seg)) = List.nth segs (i-1) in
    fold_segs f (fold_seg f acc seg n) segs (i-1)
let fold_dict f acc dict =
  let segs = dict in
  fold_segs f acc segs (List.length segs)
let dict_to_list dict =
  fold_dict (fun k v acc -> (k, v) :: acc) [] dict

let dict_of_etf = function
  | Etf.SmallTuple(9, [
        Atom "dict";
        SmallInteger size;
        SmallInteger num_of_active_slot;
        SmallInteger maxn;
        SmallInteger buddy_slot_offset;
        SmallInteger exp_size;
        SmallInteger con_size;
        empty_segment;
        SmallTuple (_, segs);
    ]) ->
     Ok (dict_to_list segs)
  | other ->
     Error (Failure (!%"dict_of_etf"))

let fold_elist f acc = function
  | Etf.Nil -> acc
  | List(bkt, Nil) -> List.fold_left ~f ~init:acc bkt
  | other ->
     failwith (!%"fold_elist: '%s'" (show_etf other))
let fold_seg f acc0 seg =
  let[@warning "-8"] Etf.SmallTuple (_, bs) = seg in
  List.fold_left ~f:(fun acc elist -> fold_elist f acc elist) ~init:acc0 bs
let fold_segs f acc0 segs =
  List.fold_left ~f:(fun acc seg -> fold_seg f acc seg) ~init:acc0 segs
let to_list segs =
  fold_segs (fun xs x -> x :: xs) [] segs
let set_of_etf = function
  | Etf.SmallTuple(9, [
        Atom "set";
        SmallInteger size;
        SmallInteger num_of_active_slot;
        SmallInteger maxn;
        SmallInteger buddy_slot_offset;
        SmallInteger exp_size;
        SmallInteger con_size;
        empty_segment;
        SmallTuple (_, segs);
    ]) ->
     Ok (to_list segs)
  | other ->
     Error (Failure (!%"set_of_etf: %s" (show_etf other)))

let file_md5_of_etf = function
  | Etf.SmallTuple(2, [
        String filename;
        Binary bin;
    ]) ->
     Ok {filename; binary = Bitstring.string_of_bitstring bin}
  | other ->
     Error (Failure (!%"file_md5_of_etf error: %s" (show_etf other)))

let mfa_of_etf = function
  | Etf.SmallTuple(3, [
        Atom module_;
        Atom func;
        SmallInteger arity;
    ]) ->
     Ok (module_, func, arity)
  | other ->
     Error (Failure (!%"mfa_of_etf error: %s" (show_etf other)))
let infoval_of_etf = function
  | Etf.SmallTuple(2, [
        v;
        List (vs, Nil);
    ]) ->
     Ok(v,vs)
  | SmallTuple(2, [
        v;
        Nil;
    ]) ->
     Ok(v,[])
  | other ->
     Error (Failure (!%"infoval_of_etf error: %s" (show_etf other)))
