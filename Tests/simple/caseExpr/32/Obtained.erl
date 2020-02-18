-module('Obtained').

-export([decision/2]).

decision(X, _) ->
    case X of
        2 ->
            X + undef
    end.