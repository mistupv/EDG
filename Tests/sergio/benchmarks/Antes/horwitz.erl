-module(horwitz).
-export([main/0]).

main() -> 
  Sum = 0,
  I = 1,
  {Result,_} = while(Sum,I,11),
  Result.

while(S,I,Top) ->
  if Top /= 0 ->
        NewS = add(S,I),
        NewI = increment(I),
        while(NewS,NewI,Top-1);
      Top == 0 ->
        {S,I}
  end.

add(A,B) -> A+B.
increment(A) -> add(A,1).

%Node Id = 41