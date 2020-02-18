-module('Expected').

-export([decision/2]).

decision(_, Y = _) ->
    case undef of
        Y ->
            undef
    end.