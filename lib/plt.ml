open Base
open Obeam
module Etf = External_term_format
open Common
open Ast_intf
module Format = Caml.Format

(** OTP-21.1.1 *)

type ('a, 'b) map = ('a * 'b) list
[@@deriving show]

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
  | other ->
     Error (Failure(!%"list_of_etf: '%s'" (show_etf other)))
     
(** ========================================================================= *)
(** basic erlang data structure: set *)
(** ========================================================================= *)

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
(** ========================================================================= *)
(** basic erlang data structure: dict *)
(** ========================================================================= *)
let fold_dict f init dict =
  let segs = dict in
  fold_segs (fun acc e ->
      match e with
      | List([k; v], Nil) -> f k v acc
      | List([k], v) -> f k v acc
      | other -> failwith (!%"fold_dict: %s" (show_etf other))
    )
       init segs
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

(** ========================================================================= *)
(** ========================================================================= *)

type mfa = string * string * int
[@@deriving show]

type ret_args_types = typ * typ list
[@@deriving show]

type contract = {
    contracts: (typ *  (typ*typ) list) list;
    args : typ list;
    forms: (Etf.t * Etf.t) list; (*???*)
  }
[@@deriving show]

type module_name = string

(**
```
-record(plt, {info      :: ets:tid(), %% {mfa() | integer(), ret_args_types()}
              types     :: ets:tid(), %% {module(), erl_types:type_table()}
              contracts :: ets:tid(), %% {mfa(), #contract{}}
              callbacks :: ets:tid(), %% {module(),
                                      %%  [{mfa(),
                                      %%  dialyzer_contracts:file_contract()}]
              exported_types :: ets:tid() %% {module(), sets:set()}
             }).
 ```
https://github.com/erlang/otp/blob/OTP-21.1.1/lib/dialyzer/src/dialyzer_plt.erl#L78
 *)
type info_key = InfoKey_Mfa of mfa | InfoKey_Int of int
type t = {
    info : (info_key, ret_args_types) map;
    types : (module_name, ret_args_types) map;
    contracts : (mfa, ret_args_types) map;
  }

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
    info : (mfa, ret_args_types) map;
    contracts : (mfa, contract) map;
    callbacks : (Etf.t, Etf.t) map;
    types : (Etf.t, Etf.t) map;
    exported_types : Etf.t list;
    mod_deps : (Etf.t, Etf.t) map;
    implementation_md5 : file_md5 list;
  }
[@@deriving show]

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
        Atom module_name;
        Atom func;
        SmallInteger arity;
    ]) ->
     Ok (module_name, func, arity)
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

(**
```
-record(c, {tag			      :: tag(),
	    elements  = []	      :: term(),
	    qualifier = ?unknown_qual :: qual()}).

-opaque erl_type() :: ?any | ?none | ?unit | #c{}.
```
https://github.com/erlang/otp/blob/OTP-21.1.1/lib/hipe/cerl/erl_types.erl
 *)
    
let erl_type_of_etf = function
  | Etf.Atom "any" -> Ok TyAny
  | Atom "none" -> Ok TyNone
  | Atom "unit" -> Ok (TyConstant(Atom"unit")) (* TODO *)
  | SmallTuple(4, [
                 Atom "c";
                 tag;
                 elements;
                 qualifier;
              ]) as etf ->
     Ok (TyConstant(String (show_etf etf))) (*TODO*)
  | other ->
     Error (Failure (!%"erl_type_of_etf error: %s" (show_etf other)))
               
    ]) ->
     Ok(v,[])
  | other ->
     Error (Failure (!%"infoval_of_etf error: %s" (show_etf other)))
