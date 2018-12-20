open Base
open Fialyzer
module Arg = Caml.Arg

type param = {
    beam_file : string;
    plt_file : string
  }

let plt_file_ref = ref ""

let usage_msg = "Usage: fialyzer <beam_filename>"

let work f =
  let specs = [
      ("--plt", Arg.Set_string plt_file_ref, "Use the specified plt as the initial plt");
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
