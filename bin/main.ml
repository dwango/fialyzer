open Obeam
open Base
open Result
open Fialyzer

let extract_debug_info_buf filename layout =
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
          let msg = "abst and dbgi chunk is not found" in
          raise Known_error.(FialyzerError (InvalidBeam (filename, msg)))
     end

let beam_to_etf filename beam_buf = match Chunk.parse_layout beam_buf with
  | Ok (layout, _) ->
     let debug_info_buf = extract_debug_info_buf filename layout in
     begin
       match External_term_format.parse debug_info_buf with
       | Ok (expr, _) ->
          Ok expr
       | Error (msg, _rest) ->
          Error Known_error.(FialyzerError (InvalidBeam (filename, msg)))
     end
  | Error (msg, _rest) ->
     Error Known_error.(FialyzerError (InvalidBeam (filename, msg)))

let read_file beam_filename =
  try_with (fun () -> Bitstring.bitstring_of_file beam_filename) >>= fun beam ->
  beam_to_etf beam_filename beam >>= fun etf ->
  let sf = Simple_term_format.of_etf etf in
  Ok (Abstract_format.of_sf sf)

let check_module beam_filename =
  read_file beam_filename >>= fun code ->
  From_erlang.code_to_expr code >>= fun expr ->
  (Derivation.derive (Context.init ()) expr |> map_error ~f:(fun msg -> Failure msg)) >>= fun (_ty, c) ->
  Solver.solve Solver.init c >>= fun _sol ->
  Ok ()

let () =
  Cui.work (fun param ->
      try
        Result.ok_exn begin
            check_module param.Cui.beam_file
          end;
        Caml.print_endline "done (passed successfully)"
      with
      | Known_error.FialyzerError err ->
         Caml.prerr_endline (Known_error.to_message err);
         Caml.exit 1
      | exn ->
         raise exn)
