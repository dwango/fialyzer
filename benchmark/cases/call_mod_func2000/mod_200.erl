
-module(mod_200).
-export([f/1]).

-spec f(input_200) -> ok.
f(input_200) -> mod_199:f(input_199).
