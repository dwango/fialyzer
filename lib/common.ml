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
let (>>>) f g = g <<< f
