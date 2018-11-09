(**
   plt file reader and writer (OTP-21.1.1)
   @see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/dialyzer/src/dialyzer_plt.erl>

   TODO:
   - read info
   - read types
   - read callbacks
   - read exported_types
   - writer
 *)

open Base
open Obeam
module Etf = External_term_format
open Common
open Ast_intf
module Format = Caml.Format

type etf = Etf.t
let sexp_of_etf etf = sexp_of_string (Etf.show etf)

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
     
(** =========================================================================
    Basic erlang data structure: set
    ========================================================================= *)

(** Erlang [set()] type
    @see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/stdlib/src/sets.erl#L62-L72>
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
let to_list segs =
  fold_segs ~f:(fun xs x -> x :: xs) ~init:[] segs

(** =========================================================================
    Basic erlang data structure: dict
    ========================================================================= *)

(** erlang dict() type
    @see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/stdlib/src/dict.erl#L61-L71>
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
      | other -> failwith (!%"fold_dict: %s" (Etf.show other))
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
     Error (Failure (!%"file_md5_of_etf error: %s" (Etf.show other)))

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
     Error (Failure (!%"file_plt_of_etc: %s" (Etf.show other)))

(** =========================================================================
    PLT
    ========================================================================= *)



(**
{v
-record(c, {tag			      :: tag(),
	    elements  = []	      :: term(),
	    qualifier = ?unknown_qual :: qual()}).

-opaque erl_type() :: ?any | ?none | ?unit | #c{}.
v}
@see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/hipe/cerl/erl_types.erl>
 *)
type erl_type = typ (*TODO*)
[@@deriving show, sexp_of]

type ret_args_types = erl_type * erl_type list
[@@deriving show]

(**
{v
-type contr_constr()  :: {'subtype', erl_types:erl_type(), erl_types:erl_type()}.
-type contract_pair() :: {erl_types:erl_type(), [contr_constr()]}.

-record(contract, {contracts	  = []		   :: [contract_pair()],
		   args		  = []		   :: [erl_types:erl_type()],
		   forms	  = []		   :: [{_, _}]}).
v}
@see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/dialyzer/src/dialyzer.hrl>
 *)
type contract = {
    contracts: (erl_type * (erl_type*erl_type) list) list;
    args : erl_type list;
    forms: unit; (*TODO: (Etf.t * Etf.t) list; (*???*)*)
  }
[@@deriving show, sexp_of]

type module_name = string

type info_key = InfoKey_Mfa of Mfa.t | InfoKey_Int of int [@@deriving sexp_of]

(**
{v
-record(plt, {info      :: ets:tid(), %% {mfa() | integer(), ret_args_types()}
              types     :: ets:tid(), %% {module(), erl_types:type_table()}
              contracts :: ets:tid(), %% {mfa(), #contract{}}
              callbacks :: ets:tid(), %% {module(),
                                      %%  [{mfa(),
                                      %%  dialyzer_contracts:file_contract()}]
              exported_types :: ets:tid() %% {module(), sets:set()}
             }).
v}
@see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/dialyzer/src/dialyzer_plt.erl#L78>
 *)
type t = {
    info : unit; (*TODO (info_key, ret_args_types) map;*)
    types : unit; (*TODO (module_name, ret_args_types) map;*)
    contracts : contract Poly_map.OnMfa.t;
    callbacks : unit; (*TODO*)
    exported_types : unit; (*TODO*)
  }
[@@deriving sexp_of]

let mfa_of_etf = function
  | Etf.SmallTuple(3, [
        Atom module_name;
        Atom func;
        SmallInteger arity;
    ]) ->
     Ok (module_name, func, arity)
  | other ->
     Error (Failure (!%"mfa_of_etf error: %s" (Etf.show other)))

    
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
     Ok (TyConstant(String (Etf.show etf))) (*TODO*)
  | other ->
     Error (Failure (!%"erl_type_of_etf error: %s" (Etf.show other)))
               
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
     Error (Failure (!%"ret_args_types_of_etf error: %s" (Etf.show other)))

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
     Error (Failure (!%"contr_constr_of_etf error: %s" (Etf.show other)))

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
     (*TODO: result_map_m ~f:pair_of_etf form_etfs >>= fun forms -> *)
     Ok {contracts; args; forms=()}
  | other ->
     Error (Failure (!%"contract_of_etf error: %s" (Etf.show other)))

let contracts_of_dict dict =
  let open Result in
  fold_dict ~f:(fun k v acc ->
              acc >>= fun map ->
              mfa_of_etf k >>= fun mfa ->
              contract_of_etf v >>= fun contract ->
              Ok (Map.set map ~key:mfa ~data:contract))
            ~init:(Ok(Poly_map.OnMfa.empty)) dict

let of_file_plt (file_plt: file_plt) =
  let open Result in
  contracts_of_dict file_plt.contracts >>= fun contracts ->
  Ok {info = (); contracts; types=(); callbacks=(); exported_types=()}

let of_file filename =
  let open Result in
  try_with (fun () -> Bitstring.bitstring_of_file filename) >>= fun bin ->
  (External_term_format.parse bin |> Result.map_error ~f:(fun (msg,_) -> Failure msg)) >>= fun (etf, _) ->
  file_plt_of_etf etf >>= fun file_plt ->
  of_file_plt file_plt
