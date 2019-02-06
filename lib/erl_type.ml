open Base
module Format = Caml.Format
module Etf = Obeam.External_term_format
module E = Etf_util
open Common
open Polymorphic_compare

type qualifier = FloatQual | IntegerQual | NonemptyQual | PidQual | PortQual
                 | ReferenceQual | UnknownQual
                 (* | OtherQual of etf * etf *)
[@@deriving show, sexp_of]

let qualifier_of_etf etf =
  let open Result in
  let qual_of_atom = function
    | "float" -> Ok FloatQual
    | "integer" -> Ok IntegerQual
    | "nonempty" -> Ok NonemptyQual
    | "pid" -> Ok PidQual
    | "port" -> Ok PortQual
    | "reference" -> Ok ReferenceQual
    | "unknown" -> Ok UnknownQual
    | other -> Error (Failure(!%"qual_of_atom: unknown : %s" other))
  in
  E.atom_of_etf etf >>= fun atom ->
  qual_of_atom atom

type number =
  | IntRange of {min: min; max: max}
  | IntSet of int list
  | AnyInteger (* integer() *)
  | AnyFloat   (* float()   *)
  | AnyNumber     (* number()  *)
and min = Min of int | NegativeInfinity
and max = Max of int | Infinity
[@@deriving show, sexp_of]


type tag = AtomTag | BinaryTag | FunctionTag | IdentifierTag | ListTag | MapTag
           | MatchstateTag | NilTag | NumberTag | OpaqueTag | ProductTag
           | TupleTag | TupleSetTag | UnionTag | VarTag
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

type ident_type = IPort | IPid | IReference
[@@deriving show, sexp_of]

let ident_type_of_etf etf =
  let open Result in
  let tag_of_atom = function
    | "port" -> Ok IPort
    | "pid" -> Ok IPid
    | "reference" -> Ok IReference
    | other -> Error (Failure(!%"ident_type_of_atom: unknown : %s" other))
  in
  E.atom_of_etf etf >>= fun atom ->
  tag_of_atom atom

type var_id =
  | VAtom of string
  | VInt of int
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
  | AnyAtom
  | AtomUnion of string list
  | Binary of {unit: int; base:int}
  | Function of {params: t list; ret: t}
  | AnyIdentifier
  | IdentifierUnion of ident_type list
  | List of {elem_type: t; terminal_type: t; is_nonempty: bool}
  | Nil
  | Number of number
  | AnyMap
  | Map of {map_pairs: map_pair list; dict: key_value_pair} (* Map can be used as a map or a dictionary, or both *)
  | OpaqueUnion of opaque list
  | Var of var_id
  | AnyTuple
  | Tuple of tuple
  | NTuplesUnion of n_tuples list
  (* | Matchstate of p * slots : TODO *)
  | Union of t list
and map_pair =
  | MandatoryPair of key_value_pair
  | OptionalPair of key_value_pair
and key_value_pair = {key: t; value: t}
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
and n_tuples = {
  n: int;
  tuples: tuple list
}
[@@deriving show, sexp_of]

let rec of_etf = function
  | Etf.Atom "any" -> Ok Any
  | Atom "none" -> Ok None
  | Atom "unit" -> Ok Unit
  | SmallTuple(4, [
                 Etf.Atom "c";
                 tag_etf;
                 elements;
                 qualifier_etf;
              ]) as etf ->
     let open Result in
     tag_of_etf tag_etf >>= fun tag ->
     begin match tag with
     | AtomTag ->
        begin match elements with
        | Etf.Atom "any" ->
            Ok (AnyAtom)
        | _ ->
           E.list_of_etf elements>>= fun elems ->
           result_map_m ~f:E.atom_of_etf elems >>= fun atoms ->
           Ok (AtomUnion atoms)
        end
     | BinaryTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:E.int_of_etf elems >>= fun pair ->
        begin match pair with
        | [unit; base] ->
            Ok (Binary {unit; base})
        | _ -> Error (Failure (!%"Please report: unexpected binary type '%s' in a type contract" (E.show_etf etf)))
        end
     | FunctionTag ->
        E.list_of_etf elements >>= fun elems ->
        result_guard ~error:(Failure "erl_type_of_erl:FunctionTag") (List.length elems = 2) >>= fun _ ->
        let params_etf = List.nth_exn elems 0 in
        let ret_etf = List.nth_exn elems 1 in
        of_etf ret_etf >>= fun ret ->
        begin match params_etf with
        | SmallTuple(4, [Etf.Atom "c";
                         Etf.Atom "product";
                         params_list_etf;
                         _]) ->
           E.list_of_etf params_list_etf >>= fun params_etf ->
           result_map_m ~f:of_etf params_etf >>= fun params ->
           Ok (Function {params; ret})
        | Atom "any" ->
           Ok (Function {params=[]; ret})
        | other ->
           Error(Failure (!%"tyfun_of_etf: unsupported"))
        end
     | IdentifierTag ->
        begin match elements with
        | Etf.Atom "any" ->
            Ok (AnyIdentifier)
        | _ ->
            (E.list_of_etf elements @? "Identifier") >>= fun elems ->
            result_map_m ~f:ident_type_of_etf elems >>= fun identifiers ->
            Ok (IdentifierUnion identifiers)
        end
     | ListTag ->
        E.list_of_etf elements >>= fun elems ->
        begin match elems with
        | [elem_type_etf; terminal_type_etf] ->
           of_etf elem_type_etf >>= fun elem_type ->
           of_etf terminal_type_etf >>= fun terminal_type ->
           qualifier_of_etf qualifier_etf >>= fun qualifier ->
           let is_nonempty = begin match qualifier with
           | NonemptyQual -> true
           | _ -> false
           end in
           Ok (List {elem_type; terminal_type; is_nonempty})
        | _ ->
           Error (Failure (!%"ListTag:%s" (E.show_etf elements)))
        end
     | NilTag ->
        Ok Nil
     | NumberTag ->
        E.atom_of_etf qualifier_etf >>= fun qual ->
        begin match (qual, E.atom_of_etf elements) with
        | ("float", Ok "any") ->
           Ok (Number AnyFloat)
        | ("unknown", Ok "any") ->
           Ok (Number AnyNumber)
        | ("integer", Ok "any") ->
           Ok (Number AnyInteger)
        | ("integer", Error _) ->
           E.tuple_of_etf elements >>= fun elems ->
           begin match elems with
           | [Atom "int_set"; set_etf] ->
              (E.list_of_etf set_etf >>= result_map_m ~f:E.int_of_etf) >>= fun set ->
              Ok (Number (IntSet set))
           | [Atom "int_rng"; min_etf; max_etf] ->
              begin match min_etf with
              | Atom "neg_inf" -> Result.Ok NegativeInfinity
              | _ -> Result.map ~f:(fun min -> Min min) (E.int_of_etf min_etf)
              end >>= fun min ->
              begin match max_etf with
              | Atom "pos_inf" -> Result.Ok Infinity
              | _ -> Result.map ~f:(fun max -> Max max) (E.int_of_etf max_etf)
              end >>= fun max ->
              Ok (Number (IntRange {min; max}))
           | _ ->
              Error (Failure (!%"NumberTag: unexpected int elem: %s" (E.show_etf elements)))
           end
        | _ ->
           Error (Failure (!%"NumberTag: %s" (E.show_etf etf)))
        end
     | MapTag ->
        E.tuple_of_etf elements >>= fun elems ->
        begin match elems with
        | [map_pairs_etf; dict_key_etf; dict_value_etf] ->
           of_etf dict_key_etf >>= fun dict_key ->
           of_etf dict_value_etf >>= fun dict_value ->
           E.list_of_etf map_pairs_etf >>= fun pair_etfs ->
           result_map_m ~f:map_pair_of_etf pair_etfs >>= fun map_pairs ->
           let dict = {key=dict_key; value=dict_value} in
           begin match (map_pairs, dict_key, dict_value) with
           | ([], Any, Any) -> Ok AnyMap
           | _ -> Ok (Map {map_pairs; dict})
           end
        | _ ->
           Error (Failure "erl_types(MapTag)")
        end
     | OpaqueTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:opaque_of_etf elems >>= fun opaques ->
        Ok (OpaqueUnion opaques)
     | TupleTag ->
        E.pair_of_etf qualifier_etf >>= fun (arity_etf, tag_etf) ->
        begin match elements, arity_etf, tag_etf with
        | Etf.Atom "any", Etf.Atom "any", Etf.Atom "any" -> (* tuple() *)
            Ok (AnyTuple)
        | _ ->
           E.list_of_etf elements >>= fun elems ->
           result_map_m ~f:of_etf elems >>= fun types ->
           E.int_of_etf arity_etf >>= fun arity ->
           of_etf tag_etf >>= fun tag_t ->
           begin match tag_t with
           | Any ->
              Ok (Tuple {types; arity; tag=None})
           | AtomUnion [atom] ->
              Ok (Tuple {types; arity; tag=Some atom})
           | other ->
              Error (Failure (!%"Please report: unexpected tag of tuple: '%s'" (E.show_etf tag_etf)))
           end
        end
     | VarTag ->
        result_or
          (E.int_of_etf elements >>| fun i -> VInt i)
          (E.atom_of_etf elements >>| fun id -> VAtom id)
        >>= fun id ->
        Ok (Var id)
     | TupleSetTag ->
        E.list_of_etf elements >>= fun elems ->
        let tuple_of_etf etf =
          Result.(
            of_etf etf >>= function
            | Tuple tuple -> Ok tuple
            | _ -> Error (Failure (!%"Please report: an element of tuple_set is not a tuple: %s" (E.show_etf etf)))
          )
        in
        let n_tuples_of_etf etf =
          Result.(
            E.pair_of_etf etf >>= fun (arity_etf, tuples_etf) ->
            E.int_of_etf arity_etf >>= fun n ->
            E.list_of_etf tuples_etf >>= fun etfs ->
            result_map_m ~f:tuple_of_etf etfs >>= fun tuples ->
            Ok {n; tuples}
          )
        in
        result_map_m ~f:n_tuples_of_etf elems >>= fun n_tuples_list ->
          Ok (NTuplesUnion n_tuples_list)
     | UnionTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:of_etf elems >>= fun tys ->
        Ok (Union tys)
     | other ->
        !%"of_etf: unsupported: %s\n  elements = %s\n  qual = %s" (show_tag other)
          (E.show_etf elements)
          (E.show_etf qualifier_etf)
        |> fun msg -> Error (Failure msg)
     end
  | other ->
     Error (Failure (!%"of_etf error: %s" (E.show_etf other)))
and opaque_of_etf elem =
  let open Result in
  E.tuple_of_etf elem >>= function
  | [Atom "opaque"; Atom mod_; Atom name; args_etf; struct_etf] ->
     E.list_of_etf args_etf >>= fun arg_etfs ->
     result_map_m ~f:of_etf arg_etfs >>= fun args ->
     of_etf struct_etf >>= fun struct_ ->
     Ok {mod_; name; args; struct_}
  | other ->
     Error (Failure "opaque_of_etf")
and map_pair_of_etf map_pair_etf =
  let open Result in
  E.tuple_of_etf map_pair_etf >>= fun map_pair ->
  match map_pair with
  | [key_etf; mandatoriness_etf; value_etf] ->
     of_etf key_etf >>= fun key ->
     of_etf value_etf >>= fun value ->

     begin match mandatoriness_etf with
       | Etf.Atom "mandatory" -> Ok (MandatoryPair {key; value})
       | Atom "optional" -> Ok (OptionalPair {key; value})
       | other -> Error (Failure (!%"map_pair_of_etf mandatoriness error: %s" (E.show_etf other)))
     end
  | _ ->
     Error (Failure "map_pair_of_etf")
