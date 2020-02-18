-module(mug).
-export([mug/3]).

-define(CONSTANT_F,rand:uniform(20)).
-define(CONSTANT_G,rand:uniform(30)).

mug(I,C,X) ->
	case p(I) of
		true ->
			{NewX,NewC} = 
				case q(C) of
					true ->
						TempX = ?CONSTANT_F,
						TempC = ?CONSTANT_G,
						{TempX,TempC};
					false ->
						{X,C}
			end,
			NewI = h(I),
			mug(NewI,NewC,NewX);
		false ->
			io:format("~p\n",[X]) %Slice X
	end.

p(I) ->
	I < 10.

q(C) ->
	C < 20.

h(I) ->
	I + 1.