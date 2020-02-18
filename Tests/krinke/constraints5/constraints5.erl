-module(constraints5).
-export([function/0]).

function() ->
	Z = [ 3, 4 ],
	X = [ 1, 2 ],
	Y = function1(X, Z),
	{ A, B } = Y,
	C = B ++ A.

function1(F, G) ->
	{ F, G }.