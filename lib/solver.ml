open Base
open Types

type ty_var = string
type sol = (ty_var, typ, String.comparator_witness) Map.t

let solve_eq sol ty1 ty2 = Ok sol (*TODO : see TAPL https://dwango.slack.com/messages/CD453S78B/ *)
let solve_sub sol ty1 ty2 = Ok sol (*TODO*)

let rec solve sol = function
  | Empty -> Ok sol
  | Eq (ty1, ty2) ->
     solve_eq sol ty1 ty2
  | Subtype (ty1, ty2) ->
     solve_sub sol ty1 ty2
  | Conj cs ->
     solve_conj sol cs
  | Disj cs ->
     Error (Failure "unimplemented : solving Disjunction of constraints")
and solve_conj sol = function
  | [] -> Ok sol
  | c :: cs ->
     let open Result in
     solve sol c >>= fun sol' ->
     solve_conj sol' cs
