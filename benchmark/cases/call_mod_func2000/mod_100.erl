
-module(mod_100).
-export([f/1]).

-spec f(input_100) -> ok.
f(input_100) -> mod_99:f(input_99).
