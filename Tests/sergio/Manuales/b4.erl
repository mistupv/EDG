-module(b4).
-export([numbers/2]).
numbers(A,B) -> 		
	C=A+B-3,		
	D=fn(A,B,C).
fn(A,B,C) ->
	case A of 
		3 -> 
			A+B;
		_ -> 
			gn(C) 
	end.
gn(0) -> 
	1;
gn(X) -> 
	7.