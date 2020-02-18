-module(tuples).
-export([main/0]).

main() ->
   X = {1,2},
   {A,B} = X. % Slice BW - NITC
