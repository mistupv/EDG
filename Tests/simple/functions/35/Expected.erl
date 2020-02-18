-module('Expected').

-export([divide/2,multiply/2,substract/2,add/2]).

divide(G, _) ->
    G / undef.