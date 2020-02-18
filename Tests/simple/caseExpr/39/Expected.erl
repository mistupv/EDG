-module('Expected').

-export([decision/2]).

decision(X, _ = _) ->
    case X of
        _ = _ ->
            undef
    end.