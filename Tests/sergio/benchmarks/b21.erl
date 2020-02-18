-module(b21).
-export([word_count/1]).

-spec word_count(string()) -> any().
word_count(Text) ->
	word_count(Text,0,0,0,false).

word_count([],Chars,Lines,Words,Inword) ->
	{Chars,Lines,Words,Inword};
word_count([C | Text],Chars,Lines,Words,Inword) ->
	NewChars = Chars+1,
	NewLines = 
		case C of
		 	$\n -> 
		 		Lines + 1;
			_ -> 
				Lines
		end,
	{NewWords,NewInword} = 
		case isLetter(C) of
			true -> 
				case Inword of
					false ->
						{Words+1,true};
					true ->
						{Words,Inword}
				end;
			false ->
				{Words,false}
		end,
	word_count(Text,NewChars,NewLines,NewWords,NewInword).

isLetter(C) ->
	io:format("~c ",[C]),
	if 
		((C >= $A andalso C =< $Z) 
			orelse 
		(C >= $a andalso C =< $z)) -> true;
		true -> false
	end.




	
	