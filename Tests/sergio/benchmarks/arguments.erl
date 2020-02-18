-module(arguments).
-export([main/0]).

main() ->
	C = 0,
	A = 1,
	B = C,
	D = cc(),
	C2 = 2,
	{A2,B2,D2} = p(A,B,D),
	{C3,A3,D3} = p(C2,A2,D2),
	io:format("~p",[A3]). % Slice BW

cc() ->
	1000.

p(X,Y,Z) ->
	X1 = Y / X, % Slice NITC
	Y1 = Y + 1,
	Z1 = Z + Y,
	{X1,Y1,Z1}.