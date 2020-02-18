-module(b12).
-export([main/2]).
-define(ERROR,failure).
-define(ON,enable).
-define(OFF,disable). 
light(N,L) -> 
	if 
		N > 20 andalso L==?ON -> 
			{undef, "Success switching ON"};
	    N =< 20 andalso L==?ON -> 
	    	{undef, ?ERROR};
  	    true -> 
  	   		{undef, "Success switching OFF"}
	end.
stove(P,L) -> 
	if 
		L == ?ON -> 
			{_,Reply} = light(P,L);
		true -> 
			{_,Reply} = light(P,?OFF)
	end,
	Reply.
main(N,State) when N >= -480 andalso N =< 520 -> 
	A=stove(N,State).