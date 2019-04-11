open Base

let result_map_m rs ~f = Result.all (List.map rs ~f)

let result_guard ?(error=Failure "result_guard") cond =
  if cond then Ok ()
  else Error error

let result_or r1 r2 =
  match r1 with
  | Ok _ -> r1
  | Error _ -> r2

let (@?) res message =
  let f = function
    | Failure msg -> Failure (message ^ ": " ^ msg)
    | exn -> Failure (message ^": "^Caml.Printexc.to_string exn)
  in
  Result.map_error ~f res

let (!%) = Printf.sprintf
let (<<<) = Fn.compose
let (>>>) f g = Fn.compose g f

let map_add_if_not_exists key data map =
  if not (Map.mem map key) then
    Map.add_exn ~key ~data map
  else
    map

let list_of_option o =
  o |> Option.map ~f:(fun x -> [x]) |> Option.value ~default:[]

let list_group_by ~f xs =
  let open Poly in
  let rec iter store = function
    | []  -> List.rev store
    | x :: xs ->
      let y = f x in
      let (xs', rest) = List.partition_tf ~f:(fun x' -> f x' = y) xs in
      iter ((y, x::xs') :: store) rest
  in
  iter [] xs
