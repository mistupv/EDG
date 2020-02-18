-module(switch).
-export([main/1]).

main(E) ->
   case E of 
      e1 ->
         S1;
      e2 -> 
         S2;
      e3 -> 
         S3; %Slice BW - NITC
      _ ->
      	break
   end.