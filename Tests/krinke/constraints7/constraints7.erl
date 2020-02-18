-module(constraints7).
-export([function/0]).

function() ->
	W = [ 10 | 11 ],
	X = { W,  0 },
	Y = function1(X),
	[ A | B ] = Y,
	A.
function1(X) ->
	{ A, B } = X,
	C = [ A | 0 ],
	C.