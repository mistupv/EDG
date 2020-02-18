-module(normal).
-export([function/0]).

function() ->
	Z = [ 3, 4 ],
	X = [ 1, 2 ],
	Y = { X, Z },
	{ A, B } = Y,
	C = B ++ A.