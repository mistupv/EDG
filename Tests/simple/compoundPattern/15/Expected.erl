-module('Expected').

-export([function/0]).

function() ->
    _ = {undef,undef,undef,[6|undef]}.