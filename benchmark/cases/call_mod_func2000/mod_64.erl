
-module(mod_64).
-export([f/1]).

-spec f(input_64) -> ok.
f(input_64) -> mod_63:f(input_63).
