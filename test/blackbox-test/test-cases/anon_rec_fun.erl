-module(anon_rec_fun).

-export([main/0]).

main() ->
    F = fun Fact(N) ->
                case N of
                    0 -> 1;
                    _ -> N * Fact(N - 1)
                end
        end,
    F.
