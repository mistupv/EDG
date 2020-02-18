-module(caseExpr).
-export([function/0]).

function() ->
	Z = [ 3, 4 ],
	X = [ 1, 2 ],
	Y = case 4 of
			3 ->
				{ Z, X };
			4 ->
				{ X, Z }
		end,
	{ A, B } = Y,
	C = B ++ A.