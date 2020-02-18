-module(b5).
-export([lists/2]).
lists(A,B) -> 
	C=fl(A,B).
fl([H1|_],[H2|T2]) -> 
	if	
		H1 >= 3 -> 
			H2;
		true -> 
			H1 + gl(T2)  
	end.
gl([]) -> 
	5;
gl([1|_]) -> 
	0;
gl([H|T]) -> 
	1.