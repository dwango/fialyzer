
-module(mod_900).
-export([f/1]).

-spec f(input_900) -> ok.
f(input_900) -> mod_899:f(input_899).
