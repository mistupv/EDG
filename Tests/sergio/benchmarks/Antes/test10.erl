%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test10.erl
%--
%-- AUTHORS: 	 Tamarit-Emartinm
%-- DATE:        2013           
%-- PUBLISHED:   https://github.com/tamarit/edd/blob/master/examples/rle (2016)
%-- DESCRIPTION
%-- The program implements the procedure to encode a string and decode another one in order
%-- to compare if both are representing the same string. The inputs will be a string and a
%-- codification. The programa will codificate the first one, decodificate the second one,
%-- decodificate the codification of the first one and then compare the values obtained
%-- with the inputs to test the values obtained and the inputs are equal.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(test10).
-compile([export_all]).
 
encode(S) ->
    doEncode(string:substr(S, 2), string:substr(S, 1, 1), 1, []).
 
doEncode([], CurrChar, Count, R) ->
    R ++ integer_to_list(Count) ++ CurrChar;
doEncode(S, CurrChar, Count, R) ->
    NextChar = string:substr(S, 1, 1),
    if
        NextChar == CurrChar ->
            doEncode(string:substr(S, 2), CurrChar, Count + 1, R);
        true ->
            doEncode(string:substr(S, 2), NextChar, 1,
                R ++ integer_to_list(Count) ++ CurrChar)
    end.
 
decode(S) ->
    doDecode(string:substr(S, 2), string:substr(S, 1, 1), []).
 
doDecode([], _, R) ->
    R;
doDecode(S, CurrString, R) ->
    NextChar = string:substr(S, 1, 1),
    IsInt = erlang:is_integer(catch(erlang:list_to_integer(NextChar))),
    if
        IsInt ->
            doDecode(string:substr(S, 2), CurrString ++ NextChar, R);
        true ->
            doDecode(string:substr(S, 2), [],
                R ++ string:copies(NextChar, list_to_integer(CurrString)))
    end.
 
test(PreEncoded,Expected) ->
	Encod=encode(PreEncoded),
	Decod=decode(Expected), 
	DecEnc=decode(encode(PreEncoded)),

    (Encod =:= Expected)
        and (Decod =:= PreEncoded) 
        and (DecEnc =:= PreEncoded).
