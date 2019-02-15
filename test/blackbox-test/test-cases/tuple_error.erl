-module(tuple_error).

-export([main/0]).

main() ->
    {ok, X} = {1, 2},
    X.
