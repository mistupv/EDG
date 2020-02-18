-module('Obtained').

-export([factorial/1]).

factorial(N) when N > 0 ->
    N * factorial(undef - undef).