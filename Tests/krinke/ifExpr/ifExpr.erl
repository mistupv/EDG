-module(ifExpr).
-export([function/0]).

function() ->
	Z = [ 3, 4 ],
	X = [ 1, 2 ],
	Y = if
			3 > 4 ->
				{ Z, X };
			5 >= 1 ->
				{ X, Z }
		end,
	{ A, B } = Y,
	C = B ++ A.