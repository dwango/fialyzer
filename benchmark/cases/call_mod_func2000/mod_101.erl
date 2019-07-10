
-module(mod_101).
-export([f/1]).

-spec f(input_101) -> ok.
f(input_101) -> mod_100:f(input_100).
