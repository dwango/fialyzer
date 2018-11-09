open Base
module Etf = Obeam.External_term_format
open Common

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
