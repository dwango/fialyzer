open Base
open Fialyzer
module Filename = Caml.Filename
module Arg = Caml.Arg

let make_tempdir () =
  let tmp_file = Filename.temp_file "xxxxx" "yyyyyy" in
  Filename.dirname tmp_file

let check_if_erlc_exists () =
  match Unix.system "which erlc > /dev/null" with
  | WEXITED status -> status = 0
  | WSIGNALED _
  | WSTOPPED _ -> false

let compile_erl_to_beam erl_file =
  let tempdir_path = make_tempdir () in
  let command = Printf.sprintf "erlc +debug_info -o %s %s" tempdir_path erl_file in
  let beam_file_name = Filename.concat tempdir_path ((Filename.chop_extension erl_file) ^ ".beam") in
  (beam_file_name, Unix.system command)

let rec compile_erl_files erl_files =
  match erl_files with
  | [] -> []
  | x :: xs ->
     let (file_name, status) = compile_erl_to_beam x in
     match status with
     | WEXITED status when status = 0 -> file_name :: compile_erl_files xs
     | _ -> failwith (Printf.sprintf "compiling the file `%s` failed" file_name)

type param = {
    beam_files : string list;
    plt_file : string option;
  }

let files_ref = ref []

let plt_file_ref = ref None

let set_plt_file plt_file =
  plt_file_ref := Some plt_file

let usage_msg = "Usage: fialyzer <beam_filename>"

let use_erl_file_ref = ref false

let work f =
  let specs = [
      ("--src", Arg.Set use_erl_file_ref, "Analyze erlang source codes directly");
      ("--plt", Arg.String set_plt_file, "Use the specified plt as the initial plt");
      ("--debug", Arg.Set Log.debug_mode , "Print debug logs");
    ]
  in
  Arg.parse specs
            (begin fun file ->
		   files_ref := file :: !files_ref;
             end)
            usage_msg;
  let param =
    if !use_erl_file_ref then
        if check_if_erlc_exists () then
          let compiled_beam_files = compile_erl_files !files_ref in
          {beam_files = compiled_beam_files; plt_file = !plt_file_ref}
        else
          failwith "to use --src option, please install `erlc` command."
    else
      {beam_files = !files_ref; plt_file = !plt_file_ref}
  in
  f param
