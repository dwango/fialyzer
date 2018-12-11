open Obeam
open Base
open Result
open Fialyzer

let extract_debug_info_buf beam_filename layout =
  let {
    Beam.cl_abst = opt_abst;
    Beam.cl_dbgi = opt_dbgi;
  } = layout in
  match opt_abst with
  | Some abst ->
     abst.Beam.abst_buf
  | None ->
     begin
       match opt_dbgi with
       | Some dbgi ->
          dbgi.Beam.dbgi_buf
       | None ->
          let message = "abst and dbgi chunk is not found" in
          raise Known_error.(FialyzerError (InvalidBeam {beam_filename; message}))
     end

let beam_to_etf beam_filename beam_buf = match Beam.parse_layout beam_buf with
  | Ok (layout, _) ->
     let debug_info_buf = extract_debug_info_buf beam_filename layout in
     begin
       match External_term_format.parse debug_info_buf with
       | Ok (expr, _) ->
          Ok expr
       | Error (message, _rest) ->
          Error Known_error.(FialyzerError (InvalidBeam {beam_filename; message}))
     end
  | Error (message, _rest) ->
     Error Known_error.(FialyzerError (InvalidBeam {beam_filename; message}))

let read_file beam_filename =
  if Caml.Sys.file_exists beam_filename then
    Ok (Bitstring.bitstring_of_file beam_filename)
  else
    Error Known_error.(FialyzerError (NoSuchFile beam_filename))

let code_of_file beam_filename =
  read_file beam_filename >>= fun beam ->
  beam_to_etf beam_filename beam >>= fun etf ->
  let sf = Simple_term_format.of_etf etf in
  Abstract_format.of_sf sf |> map_error ~f:(fun e -> Failure (Abstract_format.sexp_of_err_t e |> Sexp.to_string))

let check_module beam_filename =
  code_of_file beam_filename >>= fun code ->
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
