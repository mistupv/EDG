-module('Expected').

-export([decision/2]).

decision(X, _ = _) ->
    case X of
        2 ->
            X + undef
    end.