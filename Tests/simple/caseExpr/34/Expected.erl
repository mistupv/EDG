-module('Expected').

-export([decision/2]).

decision(X, _ = _) ->
    case X of
        2 ->
            undef + X * undef
    end.