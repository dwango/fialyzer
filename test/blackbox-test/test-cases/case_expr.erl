-module(case_expr).

-export([main/1]).

-spec main(1 | 'ok') -> 'foo' | 123.
main(X) ->
    case X of
        1 -> 'foo';
        ok -> 123
    end.
