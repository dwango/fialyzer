
-module(mod_4).
-export([f/1]).

-spec f(input_4) -> ok.
f(input_4) -> mod_3:f(input_3).
