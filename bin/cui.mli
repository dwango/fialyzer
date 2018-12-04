type param = {
    beam_file : string;
    plt_file : string
  }

val work : (param -> unit) -> unit
