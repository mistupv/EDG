-module(tuplesExpr).
-export([function/0]).

function() ->
	Tuple = { { 0, 1, 2 }, { 3, 4 }, 5 },
	{ X, Y, Z } = Tuple,
	{ { A, B, C }, { D, E }, F, 6 } = { X, Y, Z, X },
	A + B + C + D + E + F.