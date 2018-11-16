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
module E = Etf_util
open Common
open Ast_intf
module Format = Caml.Format

(** =========================================================================
    PLT File
    ========================================================================= *)

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
     Error (Failure (!%"file_plt_of_etc: %s" (Etf.show other)))

(** =========================================================================
    PLT
    ========================================================================= *)

type tag = AtomTag | BinaryTag | FunctionTag | IdentifierTag | ListTag | MapTag
           | MatchstateTag | NilTag | NumberTag | OpaqueTag | ProductTag
           | TupleTag | TupleSetTag | UnionTag | VarTag
[@@deriving show, sexp_of]

type erl_type = Erl_type.t
[@@deriving show, sexp_of]

let tag_of_etf etf =
  let open Result in
  let tag_of_atom = function
    | "atom" -> Ok AtomTag
    | "binary" -> Ok BinaryTag
    | "function" -> Ok FunctionTag
    | "identifier" -> Ok IdentifierTag
    | "list" -> Ok ListTag
    | "map" -> Ok MapTag
    | "matchstate" -> Ok MatchstateTag
    | "nil" -> Ok NilTag
    | "number" -> Ok NumberTag
    | "opaque" -> Ok OpaqueTag
    | "product" -> Ok ProductTag
    | "tuple_set" -> Ok TupleSetTag
    | "tuple" -> Ok TupleTag
    | "union" -> Ok UnionTag
    | "var" -> Ok VarTag
    | other -> Error (Failure(!%"tag_of_atom: unknown : %s" other))
  in
  E.atom_of_etf etf >>= fun atom ->
  tag_of_atom atom

type ret_args_types = erl_type * erl_type list
[@@deriving sexp_of]

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
[@@deriving sexp_of]

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

    
let rec erl_type_of_etf = function
  | Etf.Atom "any" -> Ok Erl_type.Any
  | Atom "none" -> Ok Erl_type.None
  | Atom "unit" -> Ok Erl_type.Unit
  | SmallTuple(4, [
                 Atom "c";
                 tag_etf;
                 elements;
                 qualifier_etf;
              ]) ->
     let open Result in
     tag_of_etf tag_etf >>= fun tag ->
     begin match tag with
     | AtomTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:E.atom_of_etf elems >>= fun atoms ->
        Ok (Erl_type.Atom atoms)
     | FunctionTag ->
        E.list_of_etf elements >>= fun elems ->
        result_guard (List.length elems = 2) (Failure "erl_type_of_erl:FunctionTag") >>= fun _ ->
        let domain_etf = List.nth_exn elems 0 in
        let range_etf = List.nth_exn elems 1 in
        erl_type_of_etf domain_etf >>= fun domain_ty ->
        begin match domain_ty with
        | Erl_type.Product domain ->
           erl_type_of_etf range_etf >>= fun range ->
           Ok (Erl_type.Function(domain, range))
        | other ->
           Error(Failure (!%"tyfun_of_etf: unsupported"))
        end
     | ListTag ->
        E.list_of_etf elements >>= fun elems ->
        begin match elems with
        | [types_etf; term_etf] ->
           erl_type_of_etf types_etf >>= fun types ->
           erl_type_of_etf term_etf >>= fun term ->
           (Erl_type.qualifier_of_etf qualifier_etf @? "ListTag") >>= fun size ->
           Ok (Erl_type.List (types, term, size))
        | _ ->
           Error (Failure (!%"ListTag:%s" (Etf.show elements)))
        end
     | NilTag ->
        Ok Erl_type.Nil
     | MapTag ->
        E.tuple_of_etf elements >>= fun elems ->
        begin match elems with
        | [pairs_etf; defkey_etf; defval_etf] ->
           erl_type_of_etf defkey_etf >>= fun defkey ->
           erl_type_of_etf defval_etf >>= fun defval ->
           E.list_of_etf pairs_etf >>= fun pair_etfs ->
           result_map_m ~f:t_map_pair_of_etf pair_etfs >>= fun t_map_dict ->
           Ok (Erl_type.Map (t_map_dict, defkey, defval))
        | _ ->
           Error (Failure "erl_types(MapTag)")
        end
     | OpaqueTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:opaque_of_etf elems >>= fun opaques ->
        Ok (Erl_type.Opaque opaques)
     | ProductTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:erl_type_of_etf elems >>= fun tys ->
        Ok (Erl_type.Product tys)
     | TupleTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:erl_type_of_etf elems >>= fun tys ->
        E.pair_of_etf qualifier_etf >>= fun (arity_etf, tag_etf) ->
        E.int_of_etf arity_etf >>= fun arity ->
        (erl_type_of_etf tag_etf @? !%"TupleTag(%s)" (Etf.show tag_etf)) >>= fun tag ->
        Ok (Erl_type.Tuple(tys, arity, tag))
     | UnionTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:erl_type_of_etf elems >>= fun tys ->
        Ok (Erl_type.Union tys)
     | other ->
        !%"erl_type_of_etf: unsupported: %s\n  elements = %s" (show_tag other)
          (Etf.show elements)
        |> fun msg -> Error (Failure msg)
     end
  | other ->
     Error (Failure (!%"erl_type_of_etf error: %s" (Etf.show other)))
and opaque_of_etf elem =
  let open Result in
  E.tuple_of_etf elem >>= function
  | [Atom "opaque"; Atom mod_; Atom name; args_etf; struct_etf] ->
     E.list_of_etf args_etf >>= fun arg_etfs ->
     result_map_m ~f:erl_type_of_etf arg_etfs >>= fun args ->
     erl_type_of_etf struct_etf >>= fun struct_ ->
     Ok {Erl_type.mod_; name; args; struct_}
  | other ->
     Error (Failure "opaque_of_etf")
and t_map_pair_of_etf etf =
  let open Result in
  E.tuple_of_etf etf >>= fun es ->
  match es with
  | [ty_etf1; mand_etf; ty_etf2] ->
     erl_type_of_etf ty_etf1 >>= fun ty1 ->
     erl_type_of_etf ty_etf2 >>= fun ty2 ->
     Erl_type.mandatoriness_of_etf mand_etf >>= fun mand ->
     Ok (ty1, mand, ty2)
  | _ ->
     Error (Failure "t_map_pair_of_etf")

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
       E.pair_of_etf etf >>= fun (x, y) ->
       erl_type_of_etf x >>= fun ty ->
       E.list_of_etf y >>= fun ys ->
       result_map_m ~f:contr_constr_of_etf ys >>= fun constrs ->
       Ok (ty, constrs)
     in
     E.list_of_etf contracts_etf >>= fun contract_etfs ->
     result_map_m ~f:elem_of_etf contract_etfs >>= fun contracts ->
     E.list_of_etf args_etf >>= fun arg_etfs ->
     result_map_m ~f:erl_type_of_etf arg_etfs >>= fun args ->
     E.list_of_etf forms_etf >>= fun form_etfs ->
     (*TODO: result_map_m ~f:pair_of_etf form_etfs >>= fun forms -> *)
     Ok {contracts; args; forms=()}
  | other ->
     Error (Failure (!%"contract_of_etf error: %s" (Etf.show other)))

let contracts_of_dict dict =
  let open Result in
  E.fold_dict ~f:(fun k v acc ->
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
  (External_term_format.parse bin |> Result.map_error ~f:(fun (msg,_) -> Failure (!%"Etf.parse: %s"msg))) >>= fun (etf, _) ->
  file_plt_of_etf etf >>= fun file_plt ->
  of_file_plt file_plt
