-module(dataEdges).
-export([f/2,g/4,h/3]).


f(0,0) -> 0;
f(X,0) -> X.

g(X,Y,{X,Z,Z},Z) when X>5;X==0,Y>Z->
      A = (fun (0,_)->0
           end)(0,X),
      f(A,0).
      
h(X,Y=X,Z)-> case (D=if Y==Z->X;true->Z end) of
				  B=Z -> A=D;
				  _ -> A=Z,B=Y
		     end,
		     E=X*Y,
                     F=i,
		     F(E,3),
		     {A,D,Y,F,B}.
		     
		 
		    
i(X,Y=3) when X>5-> A=X,
           B=A*X,
           C=A-Y,
           D={A,B,C,X,Y},
           {E,E,E}=if
           	A==B -> D,B;
           	C==X -> Y;
           	true -> A
           end,
           (fun (0,_)->0 end)(0,3).