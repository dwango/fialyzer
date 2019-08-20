-module(case_expr).

-export([main/1, three_tuple/1]).

-spec main(1 | 'ok') -> 'foo' | 123.
main(X) ->
    case X of
        1 -> 'foo';
        ok -> 123
    end.

%% three tuple pattern: https://github.com/yutopp/obeam/issues/101
-spec three_tuple({abc, integer(), atom()}) -> any().
three_tuple(X) ->
    case X of
        {abc, Foo, Bar} -> {ok, Foo, Bar}
    end.
