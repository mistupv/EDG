-module(constraints8).
-export([function/0]).

function(X) ->
	Y = foo(X),
	{ A, B } = Y,
	A.
foo(X) ->
	[ H | T ] = X,
	[ A | B ] = T,
	Y = { A, B },
	Y.