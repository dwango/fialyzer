open Obeam
open Base
open Result
open Fialyzer
open Common

let extract_debug_info_buf layout =
  let {
    Chunk.cl_abst = opt_abst;
    Chunk.cl_dbgi = opt_dbgi;
  } = layout in
  match opt_abst with
  | Some abst ->
     abst.Chunk.abst_buf
  | None ->
     begin
       match opt_dbgi with
       | Some dbgi ->
          dbgi.Chunk.dbgi_buf
       | None ->
          failwith "abst and dbgi chunk is not found"
     end

let beam_to_etf beam_buf = match Chunk.parse_layout beam_buf with
  | Ok (layout, _) ->
     let debug_info_buf = extract_debug_info_buf layout in
     begin
       match External_term_format.parse debug_info_buf with
       | Ok (expr, _) ->
          Ok expr
       | Error (msg, _rest) ->
          Error (Failure msg)
     end
  | Error (msg, _rest) ->
     Error (Failure msg)

let show_usage () =
  Printf.sprintf "Usage: %s <beam_filename>" Sys.argv.(0)

let read_file beam_filename =
  try_with (fun () -> Bitstring.bitstring_of_file beam_filename) >>= fun beam ->
  beam_to_etf beam >>= fun etf ->
  let sf = Simple_term_format.of_etf etf in
  Ok (Abstract_format.of_sf sf)

let read_input () =
  match Sys.argv with
  | [|_; beam_filename|] ->
     read_file beam_filename
  | _ ->
     Error (Failure (show_usage()))

let main =
  read_input () >>= fun code ->
  Caml.print_endline (!%"code: %s" (Abstract_format.show code));
  From_erlang.code_to_expr code >>= fun expr ->
  Caml.print_endline (!%"expr: %s" (Ast_intf.string_of_expr expr));
  (Derivation.derive Context.empty expr |> map_error ~f:(fun msg -> Failure msg)) >>= fun (ty, c) ->
  Caml.print_endline (!%"type: %s" (Ast_intf.string_of_typ ty));
  Caml.print_endline (!%"constraint:\n%s" (Ast_intf.string_of_constraint c));
  Solver.solve Solver.init c >>= fun sol ->
  Caml.print_endline (!%"solution:\n%s" (Solver.string_of_sol sol));
  Ok ()

let () =
  match main with
  | Ok () -> ()
  | Error (Failure msg) ->
     Caml.prerr_endline msg
  | Error exn ->
     raise exn
