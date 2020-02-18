-module('Expected').

-export([function/1]).

function(X) ->
    if
        X < 5 ->
            {undef,X}
    end.