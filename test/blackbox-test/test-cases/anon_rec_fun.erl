-module(anon_rec_fun).

-export([main/0]).

-spec main() -> fun((number()) -> number()).
main() ->
    F = fun Fact(N) ->
                case N of
                    0 -> 1;
                    _ -> N * Fact(N - 1)
                end
        end,
    F.
