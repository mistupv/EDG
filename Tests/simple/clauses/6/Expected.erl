-module('Expected').

-export([factorial/1]).

factorial(0) ->
    1;
factorial(N) when N > 0 ->
    undef * factorial(N - 1).