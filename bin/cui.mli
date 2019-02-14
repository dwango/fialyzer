type param = {
    beam_file : string;
    plt_file : string option;
  }

val work : (param -> unit) -> unit
