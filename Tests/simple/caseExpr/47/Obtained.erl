-module('Obtained').

-export([decision/2]).

decision(X, _) ->
    case X of
        Z = _ ->
            undef + Z - undef
    end.