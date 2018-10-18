-module(test01).

-export([g/0, h/1]).

f() ->
    0.

g() ->
    10.

-spec h(integer()) -> string().
h(_) ->
    "abcdefg".
