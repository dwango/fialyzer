open Base

let result_map_m rs ~f:f = Result.all (List.map rs ~f:f)

let result_guard cond error =
  if cond then Ok ()
  else Error error

let result_or r1 r2 =
  match r1 with
  | Ok _ -> r1
  | Error _ -> r2

let (@?) res message =
  let f = function
    | Failure msg -> Failure (msg ^ ": " ^ message)
    | exn -> Failure (Caml.Printexc.to_string exn ^": "^ message)
  in
  Result.map_error ~f res

let (!%) = Printf.sprintf
let (<<<) = Fn.compose
let (>>>) f g = g <<< f
