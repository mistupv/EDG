-module(constraints6).
-export([function/0]).

function() ->
	Z = [ 3, 4 ],
	L = [ 1, 2 ],
	Y = function1(L),
	{ C, D } = Y,
	E = D ++ C.

function1(X) ->
	[ H | T ] = X,
	[ A | B ] = T,
	{ A, H }.