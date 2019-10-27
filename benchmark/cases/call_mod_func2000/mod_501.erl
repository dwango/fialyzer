
-module(mod_501).
-export([f/1]).

-spec f(input_501) -> ok.
f(input_501) -> mod_500:f(input_500).
