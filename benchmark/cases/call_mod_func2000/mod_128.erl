
-module(mod_128).
-export([f/1]).

-spec f(input_128) -> ok.
f(input_128) -> mod_127:f(input_127).
