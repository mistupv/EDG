-module(scam_mug).
-export([mug/3]).

-define(CONSTANT_F,9).
-define(CONSTANT_G,9).

mug(I,C,X) ->
  case p(I) of
    true ->
      {NewX,NewC} =
        case q(C) of
          true ->
            TempX = ?CONSTANT_F, 
            TempC = ?CONSTANT_G, % Slice NITC
            {TempX,TempC};
          false ->
            {X,C}
        end,
      NewI = h(I),
      mug(NewI,NewC,NewX);
    false ->
      io:format("~p\n",[X])  % Slice BW
    end.

p(I) ->
  I rem 2 == 1.

q(C) ->
  C rem 2 == 1.

h(I) ->
  I div 2.