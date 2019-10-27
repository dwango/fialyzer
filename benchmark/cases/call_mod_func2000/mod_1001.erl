
-module(mod_1001).
-export([f/1]).

-spec f(input_1001) -> ok.
f(input_1001) -> mod_1000:f(input_1000).
