-module(try_catch_expr).

-export([main/0]).

-spec main() -> any().
main() ->
    catch 1 + 2.
    
