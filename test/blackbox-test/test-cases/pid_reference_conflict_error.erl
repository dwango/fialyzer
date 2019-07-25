-module(pid_reference_conflict_error).
-export([main/1]).

main(X) ->
    ref_to_list(X),
    is_process_alive(X).
