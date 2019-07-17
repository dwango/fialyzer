-module(sup_pid_port).
-export([main/1]).

main(X) ->
    true = unlink(self()), % unlink : Pid | Port -> true
    true = unlink(list_to_port(X)).
