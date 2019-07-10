
-module(mod_512).
-export([f/1]).

-spec f(input_512) -> ok.
f(input_512) -> mod_511:f(input_511).
