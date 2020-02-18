-module(horwitz).
-export([main/0]).

main() -> 
          Sum = 0,
          I = 0,
          {Result,_} = while(Sum, I,11 ),
          Result.

while(Sum, I,Top) ->
     if I/=Top -> 
     		  NSum = add(Sum,I),
                  NI = (fun (Z)->add(Z,1) end)(I),
                  while(NSum,NI,Top-1);
        I==Top -> 
                  {Sum,Top}
     end.
     
add(A,0)-> A;
add(A,B) -> A+B.