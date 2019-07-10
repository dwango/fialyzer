
-module(mod_1000).
-export([f/1]).

-spec f(input_1000) -> ok.
f(input_1000) -> mod_999:f(input_999).
