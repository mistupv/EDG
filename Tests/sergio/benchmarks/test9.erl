%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test9.erl
%--
%-- AUTHORS: 	 Abhay Jain
%-- DATE:        2015           
%-- PUBLISHED:   https://github.com/tamarit/edd/tree/master/examples/complex_number (2016)
%-- DESCRIPTION
%-- The program reveives two complex numbers following the complex record structure and
%-- calculate the addition and product of them, and the negation, inversion and conjugate
%-- of the first one. The output is a tuple with the calculated values.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
 
-module(test9).
-export([calculate/2, inverse/1]).
 
-record(complex, {real, img}).
 
calculate(A,B) ->
    case is_record(A,complex) andalso is_record(B,complex)of
        false ->  %io:format("The inputs you entered are not complex numbers\n");  	  	
				  ok; %Code to uncomment the test9Generators commented cases
        true -> 
            Sum = add (A, B),
            Product = multiply (A, B),
            Negation = negation (A),
            Inversion = inverse (A),
            Conjugate = conjugate (A),
    		
            {Sum, Product, Negation, Inversion#complex.img, Conjugate}
    end.

 
add (A, B) ->
    RealPart = A#complex.real + B#complex.real,
    ImgPart = A#complex.img + B#complex.img,
    #complex{real=RealPart, img=ImgPart}.
 
multiply (A, B) ->
    RealPart = (A#complex.real * B#complex.real) - (A#complex.img * B#complex.img),
    ImgPart = (A#complex.real * B#complex.img) + (B#complex.real * A#complex.img),
    #complex{real=RealPart, img=ImgPart}.
 
negation (A) ->
    #complex{real=-A#complex.real, img=-A#complex.img}.
 
inverse (A) ->
    C = conjugate (A),
    Mod = (A#complex.real * A#complex.real) + (A#complex.img * A#complex.img),
    RealPart = C#complex.real / Mod,
    ImgPart = C#complex.img / Mod,
    #complex{real=RealPart, img=ImgPart}.
 
conjugate (A) ->
    RealPart = A#complex.real,
    ImgPart = -A#complex.img,
    #complex{real=RealPart, img=ImgPart}.
 
print (A) ->
    if A#complex.img < 0 ->
        io:format("Ans = ~p-~pi~n", [A#complex.real, A#complex.img]);
       true ->
        io:format("Ans = ~p+~pi~n", [A#complex.real, A#complex.img])
    end.
