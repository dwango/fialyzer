-module(binary_bitstring_error).
-export([main/1]).

main(X) ->
    _ = binary_to_integer(X), %% binary -> integer.
    _ = bit_size(X). %% bitstring -> integer >= 0.
