-module('Expected').

-export([function/0]).

function() ->
    _ = {undef,undef,{undef,undef,5},undef}.