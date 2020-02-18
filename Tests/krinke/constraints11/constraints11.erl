-module(constraints11).
-export([function/0]).

function() ->
	X = [ 1, 2, 3 ],
	Y = foo1(X),
	Y.

foo1([ Head | Tail ]) ->
	B = 2,
	X = { Head, B },
	Y = foo2(X),
	Z = { 0, Y },
	Z.

foo2({ X, Y }) ->
	W = { X, 1 },
	Z = { 2, 3, W },
	Z.