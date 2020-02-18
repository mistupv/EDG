-module(b3).
-export([tuples/2]).
tuples(A,_) -> 
	C=ft(A,undef).
ft({X1,X2},_) ->
	if	
		X1 > X2 -> 
			S = 5;
		true -> 
			S = X2
	end,
	S.