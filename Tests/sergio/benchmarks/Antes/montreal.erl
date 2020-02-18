-module(montreal).
-export([mbe/2]).

mbe(J,K) ->
	case p(J) of
		true ->
			{NewJ,NewK} = 
				case q(K) of
					true ->
						TempK = f1(K),
						{J,TempK};
					false ->
						TempK = f2(K),
						TempJ = f3(J),
						{TempJ,TempK}
			end,
			mbe(NewJ,NewK);
		false ->
			io:format("~p\n",[J]) %Slice J
	end.

p(J) ->
	J < 15.

q(K) ->
	K < 6.

f1(K) ->
	K + 2.

f2(K) ->
	K - 1.

f3(J) ->
	J + rand:uniform(5).