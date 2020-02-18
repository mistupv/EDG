-module(mbe).
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
                        TempK = f2(K), % Slice NITC
                        TempJ = f3(J),
                        {TempJ,TempK}
            end,
            mbe(NewJ,NewK);
        false ->
            io:format("~p\n",[J])       % Slice BW
    end.

p(J) ->
	J rem 3 == 0.

q(K) ->
    K rem 2 == 0.

f1(K) ->
    K + 1.

f2(K) ->
    K + 1.

f3(J) ->
    J + 1.