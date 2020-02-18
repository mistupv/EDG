-module(listsExpr).
-export([function/0]).

function() ->
	List = [ [ 0, 1, 2 ], [ 3, 4 ], 5 ],
	[ X, Y, Z ] = List,
	[ List = [ A, B, C ], [ D, E ], F, 6 ] = [ X, Y, Z, X ],
	A + B + C + D + E + F.