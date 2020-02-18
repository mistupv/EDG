-module(caseExpr).
-export([decision/2]).

decision(X, Y = D) ->
	case X of
		Y -> X + X;
		1 -> X + X - Y;
		2 -> X + X * Y;
		Z = _ -> X + Y + Z - D
	end.