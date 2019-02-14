open Base
open Fialyzer
module Arg = Caml.Arg

type param = {
    beam_file : string;
    plt_file : string option;
  }

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
             f {
                 beam_file;
                 plt_file = !plt_file_ref;
               }
             end)
            usage_msg
