open Base
open Obeam
module Etf = External_term_format
module E = Etf_util
open Common
open Mfa
module Format = Caml.Format

(* ==========================================================================
   PLT File
   ========================================================================== *)

type file_md5 = {
    filename : string;
    binary : string;
  }

type file_plt = {
    version : string;
    file_md5_list : file_md5 list;
    info : E.dict; (*(mfa, ret_args_types) map;*)
    contracts : E.dict; (*(mfa, contract) map;*)
    callbacks : E.dict; (*(Etf.t, Etf.t) map;*)
    types : E.dict; (*(Etf.t, Etf.t) map;*)
    exported_types : E.set; (*Etf.t list;*)
    mod_deps : E.dict; (*(Etf.t, Etf.t) map;*)
    implementation_md5 : file_md5 list;
  }

let file_md5_of_etf = function
  | Etf.SmallTuple(2, [
        String filename;
        Binary bin;
    ]) ->
     Ok {filename; binary = Bitstring.string_of_bitstring bin}
  | other ->
     Error (Failure (!%"file_md5_of_etf error: %s" (E.show_etf other)))

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
     E.dict_of_etf info_etf >>= fun info_dict ->
     E.dict_of_etf contracts_etf >>= fun contracts_dict ->
     E.dict_of_etf callbacks_etf >>= fun callbacks_dict ->
     E.dict_of_etf types_etf >>= fun types_dict ->
     E.dict_of_etf mod_deps_etf >>= fun mod_deps_dict ->
     E.set_of_etf exported_types_etf >>= fun exported_types_set ->
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
     Error (Failure (!%"file_plt_of_etf: %s" (E.show_etf other)))

(* ==========================================================================
   PLT
   ========================================================================== *)


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
    contracts: (Erl_type.t * (Erl_type.t * Erl_type.t) list) list;
    args : Erl_type.t list;
    forms: unit; (*TODO: (Etf.t * Etf.t) list; (*???*)*)
  }
[@@deriving sexp_of]

(*TODO: info
type ret_args_types = Erl_type.t * Erl_type.t list
[@@deriving sexp_of]

type info_key = InfoKey_Mfa of Mfa.t | InfoKey_Int of int [@@deriving sexp_of]

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
     Error (Failure (!%"ret_args_types_of_etf error: %s" (E.show_etf other)))
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
    Ok {module_name=module_name; function_name=func; arity=arity}
  | other ->
     Error (Failure (!%"mfa_of_etf error: %s" (E.show_etf other)))


let contr_constr_of_etf = function
  | Etf.SmallTuple(3, [
                     Atom "subtype";
                     e1; e2
                  ]) ->
     let open Result in
     Erl_type.of_etf e1 >>= fun ty1 ->
     Erl_type.of_etf e2 >>= fun ty2 ->
     Ok (ty1, ty2)
  | other ->
     Error (Failure (!%"contr_constr_of_etf error: %s" (E.show_etf other)))

let contract_of_etf = function
  | Etf.SmallTuple(4, [
                     Atom "contract";
                     contracts_etf;
                     args_etf;
                     forms_etf;
                  ]) ->
     let open Result in
     let elem_of_etf etf =
       E.pair_of_etf etf >>= fun (x, y) ->
       Erl_type.of_etf x >>= fun ty ->
       E.list_of_etf y >>= fun ys ->
       result_map_m ~f:contr_constr_of_etf ys >>= fun constrs ->
       Ok (ty, constrs)
     in
     E.list_of_etf contracts_etf >>= fun contract_etfs ->
     result_map_m ~f:elem_of_etf contract_etfs >>= fun contracts ->
     E.list_of_etf args_etf >>= fun arg_etfs ->
     result_map_m ~f:Erl_type.of_etf arg_etfs >>= fun args ->
     E.list_of_etf forms_etf >>= fun form_etfs ->
     (*TODO: result_map_m ~f:pair_of_etf form_etfs >>= fun forms -> *)
     Ok {contracts; args; forms=()}
  | other ->
     Error (Failure (!%"contract_of_etf error: %s" (E.show_etf other)))

let contracts_of_dict dict =
  let open Result in
  E.fold_dict ~f:(fun acc k v ->
              acc >>= fun map ->
              mfa_of_etf k >>= fun mfa ->
              (contract_of_etf v @? !%"(%s)" (Mfa.show mfa)) >>= fun contract ->
              Ok (Map.set map ~key:mfa ~data:contract))
            ~init:(Ok(Poly_map.OnMfa.empty)) dict

let of_file_plt (file_plt: file_plt) =
  let open Result in
  contracts_of_dict file_plt.contracts >>= fun contracts ->
  Ok {info = (); contracts; types=(); callbacks=(); exported_types=()}

let of_etf etf =
  Result.(file_plt_of_etf etf >>= of_file_plt)

let of_file filename =
  let open Result in
  try_with (fun () -> Bitstring.bitstring_of_file filename) >>= fun bin ->
  (External_term_format.parse bin |> Result.map_error ~f:(fun (msg,_) -> Failure (!%"Etf.parse: %s"msg))) >>= fun (etf, _) ->
  of_etf etf
