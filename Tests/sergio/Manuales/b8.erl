-module(b8).
-export([main/2]).
main(A,B)->
	if 
		A>B -> 
			C=f(A,B);
	    true -> 
	    	C=f(B,A)
	end,
	C.
f(X,Y) -> 
	Z= X-Y,
	W= X+Y, 
	case Z of
		W -> 
			(fun(B,E) ->
				B+E 
			end)(X,Y);
		_ -> 
			(fun(N) -> 
				N 
			end)(W)
	end.