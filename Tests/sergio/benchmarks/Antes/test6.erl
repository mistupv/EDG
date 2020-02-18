%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test6.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test bit syntax.
%-- DESCRIPTION
%-- The program reveives a binary list of bytes. It will divide this list in groups of 
%-- three bytes R,G,B and will convert each group to an hexadecimal expression. There are
%-- two different solutions to the problem.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(test6).
-export([main/1]).

main(Pixels) -> 
	%Procedure 1
	PixList = extractpixels(Pixels),
	io:format("~p\n",[PixList]),
	HexList = hexadecimalconvert(PixList),
	io:format("~p\n",[HexList]),
	%Procedure 2
	DirectHex = directconvert(Pixels),
	{HexList,DirectHex}.

extractpixels(List) -> [{R,G,B} || <<R:8,G:8,B:8>> <= List].

hexadecimalconvert(List) -> [bin_to_hex(R,G,B) || {R,G,B} <- List].

bin_to_hex(R,G,B) -> 
	<<N:24/unit:1>> = <<R,G,B>>,
	<<N2:16/unit:1>> = <<B,R>>,
	"16#"++httpd_util:integer_to_hexlist(N2),
	"16#"++httpd_util:integer_to_hexlist(N).

directconvert(BList) -> lists:reverse(convertHex(BList,[])).

convertHex(<<>>,L) -> L;
convertHex(BList,L) -> <<N:24,Rest/binary>> = BList,
			NH=httpd_util:integer_to_hexlist(N),
			convertHex(Rest,["16#"++httpd_util:integer_to_hexlist(N)|L]).
