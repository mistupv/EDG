-module(b7).
-export([tuples/2]).
tuples(A,B) ->
	D=ht(B,A).
ht({_,_,Z},{A,B})-> 
	case Z of
		_ -> 
			A+Z*B
	end.