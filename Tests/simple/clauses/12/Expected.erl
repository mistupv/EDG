-module('Expected').

-export([factorial/1]).

factorial(0) ->
    undef;
factorial(N) when N > 0 ->
    N * factorial(N - 1).