-module(compoundPattern).
-export([function/0]).

function() ->
	X = { 1, 2, { 3, 4, 5 }, [ 6, 7 ] },
	Y = { 1, 2, { 3, 4, 5 }, [ 6, 7 ] },
	X = Y.