-module(b6).
-export([tuples/2]).
tuples(A,B) ->
	C=ft(A,B).
ft({X,Y},{X,_,W}) -> 
	case Y of		 
		W -> 
			gt({1,X});	
		Y -> 
			gt({0,undef})
	end.
gt({0,_}) ->
	4;
gt(_) -> 
	16.