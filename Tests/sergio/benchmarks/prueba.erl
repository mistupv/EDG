-module(prueba).
-compile(export_all).

main(X,F) ->
    try 
        f(X)        
    catch 
        _:_ ->      
             {slice,2}      
    end.

f(X) -> 
    
    throw(true).  