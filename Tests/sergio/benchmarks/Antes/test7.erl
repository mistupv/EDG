%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test7.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test try...catch expressions.
%-- DESCRIPTION
%-- The program reveives three input values and try to do a division between two of them.
%-- If the denominator of the division is zero the exception will be catched and the 
%-- inverse division will be tried in order to get a determinate output or an 
%-- 'undefined result' token.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(test7).
-export([main/3]).

main(A,B,C) ->

	D=f(A,B,C).

f(X,Y,Z)-> 
	W=try X/Y of
		_ -> X+Y-Z;
		Z -> X-Y
	catch error:Reason -> 
			try Y/X of
				Z -> U=Y*2+X,Z;
				X -> 15;
				_ -> catch X=5, X+3
			catch error:Reason2 -> io:format("Too many zeros for me :(.~n"), 'undefined result'
			end
	end.
