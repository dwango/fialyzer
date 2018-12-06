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
  | IntRange of int option * int option (* (min or -infinity, max or infinity) *)
  | IntSet of int list
  | Integer (* integer() *)
  | Float   (* float()   *)
  | All     (* number()  *)
[@@deriving show, sexp_of]

type t_map_mandatoriness = Mandatory | Optional
[@@deriving show, sexp_of]

let mandatoriness_of_etf = function
  | Etf.Atom "mandatory" -> Ok Mandatory
  | Atom "optional" -> Ok Optional
  | other -> Error (Failure (!%"mandatoriness_of_etf error: %s" (E.show_etf other)))

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

type ident_type = IAny | IPort | IPid | IReference
[@@deriving show, sexp_of]

let ident_type_of_etf etf =
  let open Result in
  let tag_of_atom = function
    | "any" -> Ok IAny
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
  | Unit
  | Atom of string list
  | Binary of int * int (*(unit, base)*)
  | Function of t list * t
  | Identifier of ident_type list
  | List of t * t * qualifier
  | Nil
  | Number of number
  | Map of t_map_pair list * t * t
  | Opaque of opaque list
  | Product of t list
  | Var of var_id
  | Tuple of tuple option
  | TupleSet of (int * tuple list) list (* union of tuple *)
  (* | Matchstate of p * slots : TODO *)
  | Union of t list
[@@deriving show, sexp_of]
and t_map_pair = t * t_map_mandatoriness * t
[@@deriving show, sexp_of]
and opaque = {
    mod_ : string;
    name : string;
    args : t list;
    struct_ : t;
  }
[@@deriving show, sexp_of]
and tuple = {
    types : t list;      (* type of elements *)
    arity : int;         (* size *)
    tag : string option; (* first element's tag (may be record name) *)
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
           Ok (Atom ["any"])
        | _ ->
           E.list_of_etf elements>>= fun elems ->
           result_map_m ~f:E.atom_of_etf elems >>= fun atoms ->
           Ok (Atom atoms)
        end
     | BinaryTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:E.int_of_etf elems >>= fun pair ->
        begin match pair with
        | [unit; base] ->
           Ok (Binary (unit, base))
        | _ -> Error (Failure (!%"Please report: unexpected binary type '%s' in a type contract" (E.show_etf etf)))
        end
     | FunctionTag ->
        E.list_of_etf elements >>= fun elems ->
        result_guard ~error:(Failure "erl_type_of_erl:FunctionTag") (List.length elems = 2) >>= fun _ ->
        let domain_etf = List.nth_exn elems 0 in
        let range_etf = List.nth_exn elems 1 in
        of_etf domain_etf >>= fun domain_ty ->
        of_etf range_etf >>= fun range ->
        begin match domain_ty with
        | Product domain ->
           Ok (Function(domain, range))
        | Any ->
           Ok (Function([], range))
        | other ->
           Error(Failure (!%"tyfun_of_etf: unsupported"))
        end
     | IdentifierTag ->
        (E.list_of_etf elements @? "Identifier") >>= fun elems ->
        result_map_m ~f:ident_type_of_etf elems >>= fun ident_types ->
        Ok (Identifier ident_types)
     | ListTag ->
        E.list_of_etf elements >>= fun elems ->
        begin match elems with
        | [types_etf; term_etf] ->
           of_etf types_etf >>= fun types ->
           of_etf term_etf >>= fun term ->
           (qualifier_of_etf qualifier_etf @? "ListTag") >>= fun size ->
           Ok (List (types, term, size))
        | _ ->
           Error (Failure (!%"ListTag:%s" (E.show_etf elements)))
        end
     | NilTag ->
        Ok Nil
     | NumberTag ->
        E.atom_of_etf qualifier_etf >>= fun qual ->
        begin match (qual, E.atom_of_etf elements) with
        | ("float", Ok "any") ->
           Ok (Number Float)
        | ("unknown", Ok "any") ->
           Ok (Number All)
        | ("integer", Ok "any") ->
           Ok (Number Integer)
        | ("integer", Error _) ->
           E.tuple_of_etf elements >>= fun elems ->
           begin match elems with
           | [Atom "int_set"; set_etf] ->
              (E.list_of_etf set_etf >>= result_map_m ~f:E.int_of_etf) >>= fun set ->
              Ok (Number (IntSet set))
           | [Atom "int_rng"; min_etf; max_etf] ->
              let min = E.int_of_etf min_etf |> Result.ok in
              let max = E.int_of_etf max_etf |> Result.ok in
              result_guard (Option.is_some min || min_etf = Atom "neg_inf") >>= fun () ->
              Ok (Number (IntRange (min, max)))
           | _ ->
              Error (Failure (!%"NumberTag: unexpected int elem: %s" (Etf.show elements)))
           end
        | _ ->
           Error (Failure (!%"NumberTag: %s" (Etf.show etf)))
        end
     | MapTag ->
        E.tuple_of_etf elements >>= fun elems ->
        begin match elems with
        | [pairs_etf; defkey_etf; defval_etf] ->
           of_etf defkey_etf >>= fun defkey ->
           of_etf defval_etf >>= fun defval ->
           E.list_of_etf pairs_etf >>= fun pair_etfs ->
           result_map_m ~f:t_map_pair_of_etf pair_etfs >>= fun t_map_dict ->
           Ok (Map (t_map_dict, defkey, defval))
        | _ ->
           Error (Failure "erl_types(MapTag)")
        end
     | OpaqueTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:opaque_of_etf elems >>= fun opaques ->
        Ok (Opaque opaques)
     | ProductTag ->
        E.list_of_etf elements >>= fun elems ->
        result_map_m ~f:of_etf elems >>= fun tys ->
        Ok (Product tys)
     | TupleTag ->
        E.pair_of_etf qualifier_etf >>= fun (arity_etf, tag_etf) ->
        begin match elements, arity_etf, tag_etf with
        | Etf.Atom "any", Etf.Atom "any", Etf.Atom "any" -> (* tuple() *)
           Ok (Tuple None)
        | _ ->
           E.list_of_etf elements >>= fun elems ->
           result_map_m ~f:of_etf elems >>= fun types ->
           E.int_of_etf arity_etf >>= fun arity ->
           of_etf tag_etf >>= fun tag_t ->
           begin match tag_t with
           | Any ->
              Ok (Tuple (Some{types; arity; tag=None}))
           | Atom [atom] ->
              Ok (Tuple (Some{types; arity; tag=Some atom}))
           | other ->
              Error (Failure (!%"Please report: unexpected tag of tuple: '%s'" (E.show_etf tag_etf)))
           end
        end
     | VarTag ->
        result_or
          (E.int_of_etf elements >>| fun i -> VInt i)
          (E.atom_of_etf elements >>| fun id -> VAtom id)
        >>= fun var_id ->
        Ok (Var var_id)
     | TupleSetTag ->
        E.list_of_etf elements >>= fun elems ->
        let tuple_of_etf etf =
          Result.(
            of_etf etf >>= function
            | Tuple (Some tuple) -> Ok tuple
            | _ -> Error (Failure (!%"Please report: an element of tuple_set is not a tuple: %s" (E.show_etf etf)))
          )
        in
        let tuples_of_etf etf =
          Result.(
            E.pair_of_etf etf >>= fun (arity_etf, tuples_etf) ->
            E.int_of_etf arity_etf >>= fun arity ->
            E.list_of_etf tuples_etf >>= fun etfs ->
            result_map_m ~f:tuple_of_etf etfs >>= fun tups ->
            Ok (arity, tups)
          )
        in
        result_map_m ~f:tuples_of_etf elems >>= fun tuples ->
        Ok (TupleSet tuples)
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
and t_map_pair_of_etf etf =
  let open Result in
  E.tuple_of_etf etf >>= fun es ->
  match es with
  | [ty_etf1; mand_etf; ty_etf2] ->
     of_etf ty_etf1 >>= fun ty1 ->
     of_etf ty_etf2 >>= fun ty2 ->
     mandatoriness_of_etf mand_etf >>= fun mand ->
     Ok (ty1, mand, ty2)
  | _ ->
     Error (Failure "t_map_pair_of_etf")
