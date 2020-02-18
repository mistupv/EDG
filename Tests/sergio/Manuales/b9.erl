-module(b9).
-export([main/2]).
main(A,B)->
	D=h(fun g/2,A,B).
g(X,Y) ->
	(fun(A,B) ->  
		A+B 
	end)(X,Y). 
h(F,A,B) ->  
	F(A,B).