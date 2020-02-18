-module(word_count).
-export([word_count/1]).

word_count(Text) ->
	word_count(Text,0,0,0,false).

word_count([],NC,NL,NW,Inword) ->
	io:format("~p",[NL]), % Slice BW
	io:format("~p",[NW]),
	io:format("~p",[NC]);
word_count([C | Text],NC,NL,NW,Inword) ->
	NewNC = NC+1,
	NewNL = 
		case C of
		 	$\n -> 
		 		NL + 1; % Slice NITC
			_ -> 
				NL
		end,
	{NewNW,NewInword} = 
		case ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z)) of
			true -> 
				case Inword of
					false ->
						{NW+1,true};
					true ->
						{NW,Inword}
				end;
			false ->
				{NW,false}
		end,
	word_count(Text,NewNC,NewNL,NewNW,NewInword).
