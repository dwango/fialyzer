type param = {
    beam_files : string list;
    plt_file : string option;
  }

val work : (param -> unit) -> unit
