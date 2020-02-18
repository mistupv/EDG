%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test2Slice3.erl
%--
%-- DATE:        2016 
%-- SLICING CRITERION: Variable C (LINE 47)         
%-- DESCRIPTION
%-- This program is a theoric result of applying static backward slicing to variable C at
%-- line 47 of the test2.erl file.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(code).
-export([tuples/2]).

tuples(A,B) ->
	C=ft(A,B).

ft({X,Y},{X,_,W}) -> 
	case Y of		 
		W -> gt({1,X});	
		Y -> gt({0,undef})
	end.

gt({0,_}) -> 4;
gt(_) -> 16.
