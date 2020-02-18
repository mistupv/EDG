-module(constraints9).
-export([function/0]).

function(X) ->
	C = { 1, 2 },
	D = { 3, 4 },
	Y = foo(C),
	Z = foo(D),
	[ E, F ] = Y,
	[ G, H ] = Z,
	I = E + H,
	foo2().

foo(X) ->
	{ A, B } = X,
	[ B, A ].

foo2() ->
	foo({ 1, 2, 3 }).