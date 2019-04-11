open Base
open Fialyzer
module Arg = Caml.Arg

type param = {
    beam_files : string list;
    plt_file : string option;
  }

let beam_files_ref = ref []

let plt_file_ref = ref None

let set_plt_file plt_file =
  plt_file_ref := Some plt_file

let usage_msg = "Usage: fialyzer <beam_filename>"

let work f =
  let specs = [
      ("--plt", Arg.String set_plt_file, "Use the specified plt as the initial plt");
      ("--debug", Arg.Set Log.debug_mode , "Print debug logs");
    ]
  in
  Arg.parse specs
            (begin fun beam_file ->
		   beam_files_ref := beam_file :: !beam_files_ref;
             end)
            usage_msg;
  let param = {beam_files = !beam_files_ref; plt_file = !plt_file_ref} in
  f param
