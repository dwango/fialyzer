
-module(mod_802).
-export([f/1]).

-spec f(input_802) -> ok.
f(input_802) -> mod_801:f(input_801).
