-module('Expected').

-export([decision/2]).

decision(_, _ = D) ->
    undef.