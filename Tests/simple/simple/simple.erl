-module(simple).
-export([g/1]).

f(0,0) -> 0;
f(X,0) -> X;
f(0,Y) -> Y;
f(X,Y) -> case X of
               Y -> X+X;
               _ -> X+Y
          end.

g(X) -> A = (fun (0,N)->1;
                 (Y,Z)-> if Y>Z -> Z;
                            true -> Y
                         end
             end)(0,X),
        B=f(A,X),
        C=f(A,0),
        (A+B)*C.