
-module(mod_3).
-export([f/1]).

-spec f(input_3) -> ok.
f(input_3) -> mod_2:f(input_2).
