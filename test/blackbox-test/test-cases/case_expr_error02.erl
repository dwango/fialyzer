%% Dialyzer does not judge this code as an error.
-module(case_expr_error02).

-export([main/1]).

main(X) ->
    case X of
        1 -> 'foo';
        ok -> 123
    end + 42.
