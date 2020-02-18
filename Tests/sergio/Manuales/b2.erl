-module(b2).
-export([lists/2]).
lists(A,B) -> 
	C=fl(A,B).
fl([H1|_],[H2|T2]) ->
	if	
		H1 >= 3 -> 
			H2;
		true -> 
			[H|_]=T2,
			H1-H  
	end.