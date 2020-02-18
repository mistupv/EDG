-module(calls).
-export([f/1]).


g({3,5,Z},Z,_,Z)->0.

j(X,{X,X}=X,[X]=X)->0.

h(X,X,X,X)->0.

f([X,X],0) when X==0-> 0;
f(X,0) -> X;
f(0,Y) -> Y;
f(Y=X,Y) -> X+Y;
f(X,3) -> X;
f(_,_)->0.
     
h(X,Y=X,Z)-> (case  X of
				  Z -> if Z==0->(fun i/2);true->(fun (2,3)->5; (_,_)-> 0 end) end ;
				  _ -> f
		     end)(X,0),
		     X(1,1).
		     
		  
i(3,0)->3;   
i(_,0)-> 0;
i(_,Y=3)-> Y;
i(_,Y)-> Y .

f(X) -> h((case X of 
                0 -> {1,X};
                1 -> [3+5,X];
                Y -> Y
            end),{3+2,(if X==3-> 5;true->7 end),X},3+2),
         g({3,5,X},X,3*5,X),
         f(3,3),
         X(3*2,3),
         f([3,3],0),
         i(5,8),
         f(5,3).