-module('Expected').

-export([function/1]).

function(X) ->
    if
        X < 5 ->
            {0,undef}
    end.