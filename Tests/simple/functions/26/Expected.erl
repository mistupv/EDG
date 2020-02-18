-module('Expected').

-export([divide/2,multiply/2,substract/2,add/2]).

multiply(E, _) ->
    E * undef.