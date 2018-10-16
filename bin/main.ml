open Obeam

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
       | Error (msg, rest) ->
          Error (msg, rest)
     end
  | Error (msg, rest) ->
     Error (msg, rest)

let () =
  let beam_filename =
    match Sys.argv with
    | [|_; n|] -> n
    | _ -> failwith (Printf.sprintf "format: %s <beam_filename>" Sys.argv.(0))
  in

  let beam_buf = Bitstring.bitstring_of_file beam_filename in
  match beam_to_etf beam_buf with
  | Ok etf ->
     let sf = Simple_term_format.of_etf etf in
     let code = Abstract_format.of_sf sf in
     Printf.printf "code: %s" (Abstract_format.show code);
     begin match Fialyzer.From_erlang.code_to_expr code with
     | Ok m ->
        Printf.printf "code: %s" (Fialyzer.Ast_intf.show_expr m)
     | Error exn ->
        raise exn
     end
  | Error (msg, rest) ->
     Printf.printf "Failed to parse chunk: %s\n" msg;
     Bitstring.hexdump_bitstring stdout rest
