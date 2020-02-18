-module('Obtained').

-export([factorial/1]).

factorial(_) when N > 0 ->
    undef * factorial(undef);
factorial(_) when N < 0 ->
    error.