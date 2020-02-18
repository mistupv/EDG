-module('Obtained').

-export([decision/2]).

decision(X, Y = _) ->
    case X of
        2 ->
            undef + undef * Y
    end.