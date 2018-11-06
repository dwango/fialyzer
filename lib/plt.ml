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
     
(** =========================================================================
    Basic erlang data structure: set
    ========================================================================= *)

(** erlang set() type
    https://github.com/erlang/otp/blob/OTP-21.1.1/lib/stdlib/src/sets.erl#L62-L72
 *)
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
     Error (Failure (!%"set_of_etf: %s" (show_etf other)))

let fold_elist ~f ~init = function
  | Etf.Nil -> init
  | List(bkt, Nil) -> List.fold_left ~f ~init bkt
  | other ->
     failwith (!%"fold_elist: '%s'" (show_etf other))
let fold_seg ~f ~init seg =
  let[@warning "-8"] Etf.SmallTuple (_, bs) = seg in
  List.fold_left ~f:(fun acc elist -> fold_elist ~f ~init:acc elist) ~init bs
let fold_segs ~f ~init segs =
  List.fold_left ~f:(fun acc seg -> fold_seg ~f ~init:acc seg) ~init segs
let to_list segs =
  fold_segs ~f:(fun xs x -> x :: xs) ~init:[] segs

(** =========================================================================
    Basic erlang data structure: dict
    ========================================================================= *)

(** erlang dict() type
    https://github.com/erlang/otp/blob/OTP-21.1.1/lib/stdlib/src/dict.erl#L61-L71
 *)
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
      | List([k; v], Nil) -> f k v acc
      | List([k], v) -> f k v acc
      | other -> failwith (!%"fold_dict: %s" (show_etf other))
    ) ~init segs
let dict_to_list dict =
  fold_dict ~f:(fun k v acc -> (k, v) :: acc) ~init:[] dict

(** =========================================================================
    PLT File
    ========================================================================= *)

type file_md5 = {
    filename : string;
    binary : string;
  }
[@@deriving show]

type file_plt = {
    version : string;
    file_md5_list : file_md5 list;
    info : dict; (*(mfa, ret_args_types) map;*)
    contracts : dict; (*(mfa, contract) map;*)
    callbacks : dict; (*(Etf.t, Etf.t) map;*)
    types : dict; (*(Etf.t, Etf.t) map;*)
    exported_types : set; (*Etf.t list;*)
    mod_deps : dict; (*(Etf.t, Etf.t) map;*)
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

let file_plt_of_etf = function
  | Etf.SmallTuple(10, [
        Atom "file_plt";
        String version;
        List(file_md5s, _);
        info_etf;
        contracts_etf;
        callbacks_etf;
        types_etf;
        exported_types_etf;
        mod_deps_etf;
        List(impl_md5s, _);
    ]) ->
     let open Result in
     result_map_m ~f:file_md5_of_etf file_md5s >>= fun file_md5_list ->
     result_map_m ~f:file_md5_of_etf impl_md5s >>= fun implementation_md5 ->
     dict_of_etf info_etf >>= fun info_dict ->
     dict_of_etf contracts_etf >>= fun contracts_dict ->
     dict_of_etf callbacks_etf >>= fun callbacks_dict ->
     dict_of_etf types_etf >>= fun types_dict ->
     dict_of_etf mod_deps_etf >>= fun mod_deps_dict ->
     set_of_etf exported_types_etf >>= fun exported_types_set ->
     Ok {
         version;
         file_md5_list;
         info = info_dict;
         contracts = contracts_dict;
         callbacks = callbacks_dict;
         types = types_dict;
         exported_types = exported_types_set;
         mod_deps = mod_deps_dict;
         implementation_md5;
       }
  | other ->
     Error (Failure (!%"file_plt_of_etc: %s" (show_etf other)))

(** =========================================================================
    PLT
    ========================================================================= *)

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

let mfa_of_etf = function
  | Etf.SmallTuple(3, [
        Atom module_name;
        Atom func;
        SmallInteger arity;
    ]) ->
     Ok (module_name, func, arity)
  | other ->
     Error (Failure (!%"mfa_of_etf error: %s" (show_etf other)))

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
               
let ret_args_types_of_etf = function
  | Etf.SmallTuple(2, [v; List (vs, Nil) ]) ->
     let open Result in
     erl_type_of_etf v >>= fun ty ->
     result_map_m ~f:erl_type_of_etf vs >>= fun arg_types ->
     Ok(ty, arg_types)
  | SmallTuple(2, [v; Nil]) ->
     let open Result in
     erl_type_of_etf v >>= fun ty ->
     Ok(ty, [])
  | other ->
     Error (Failure (!%"ret_args_types_of_etf error: %s" (show_etf other)))

(**
```
-type contr_constr()  :: {'subtype', erl_types:erl_type(), erl_types:erl_type()}.
-type contract_pair() :: {erl_types:erl_type(), [contr_constr()]}.

-record(contract, {contracts	  = []		   :: [contract_pair()],
		   args		  = []		   :: [erl_types:erl_type()],
		   forms	  = []		   :: [{_, _}]}).
```
https://github.com/erlang/otp/blob/OTP-21.1.1/lib/dialyzer/src/dialyzer.hrl
 *)
let contr_constr_of_etf = function
  | Etf.SmallTuple(3, [
                     Atom "subtype";
                     e1; e2
                  ]) ->
     let open Result in
     erl_type_of_etf e1 >>= fun ty1 ->
     erl_type_of_etf e2 >>= fun ty2 ->
     Ok (ty1, ty2)
  | other ->
     Error (Failure (!%"contr_constr_of_etf error: %s" (show_etf other)))

let contract_of_etf = function
  | Etf.SmallTuple(4, [
                     Atom "contract";
                     contracts_etf;
                     args_etf;
                     forms_etf;
                  ]) ->
     let open Result in
     let elem_of_etf etf =
       pair_of_etf etf >>= fun (x, y) ->
       erl_type_of_etf x >>= fun ty ->
       list_of_etf y >>= fun ys ->
       result_map_m ~f:contr_constr_of_etf ys >>= fun constrs ->
       Ok (ty, constrs)
     in
     list_of_etf contracts_etf >>= fun contract_etfs ->
     result_map_m ~f:elem_of_etf contract_etfs >>= fun contracts ->
     list_of_etf args_etf >>= fun arg_etfs ->
     result_map_m ~f:erl_type_of_etf arg_etfs >>= fun args ->
     list_of_etf forms_etf >>= fun form_etfs ->
     result_map_m ~f:pair_of_etf form_etfs >>= fun forms ->
     Ok {contracts; args; forms}
  | other ->
     Error (Failure (!%"contract_of_etf error: %s" (show_etf other)))
