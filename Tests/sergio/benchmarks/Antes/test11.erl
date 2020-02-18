%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test11.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test the define and spec instruccions.
%-- DESCRIPTION
%-- The program reveives a number and a state to swith the stove as inputs. If the state 
%-- is ON and the number is greater than twenty, the program will return a success 
%-- message, if it is ON and the number is less or equal to twenty the program will return
%-- an error message. The OFF input state will always provide a success message.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(test11).
-export([main/2]).
-define(ERROR,failure).
-define(ON,enable).
-define(OFF,disable). 

-type num() :: integer().
-type word() :: string().
-type thing() :: atom().
-type switch() :: ?ON | ?OFF.
-type reply() :: word() | ?ERROR.

-spec light(N::num(),L::switch()) -> {'ok',word()} | {'error',thing()}.

light(N,L) -> if N > 20 andalso L==?ON -> {'ok', "Success switching ON"};
				 N =< 20 andalso L==?ON -> {'error', ?ERROR};
  			     true   -> {'ok', "Success switching OFF"}
			  end.

-spec stove(P::num(),L::switch()) -> reply().

stove(P,L) -> if L == ?ON -> {Res,Reply} = light(P,L);
				 true -> {Res,Reply} = light(P,?OFF)
			  end, Reply.

main(N,State) when (N > -480 andalso N =< 520) andalso (State == ?ON orelse State == ?OFF) ->
	A=stove(N,State).
