-module('Expected').

-export([decision/2]).

decision(_, Y = _) ->
    undef.