-module('Expected').

-export([decision/2]).

decision(X, _ = _) ->
    undef.