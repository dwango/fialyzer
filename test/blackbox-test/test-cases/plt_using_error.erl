-module(plt_using_error).

-export([main/0]).

main() ->
    {{Year, Month, Day}, {Hour, hoge, Second}} = calendar:local_time(),
    0.
