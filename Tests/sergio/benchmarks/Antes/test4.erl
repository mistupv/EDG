%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test4.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test list comprehensions. 
%-- DESCRIPTION
%-- The program reveives a list of chemical elements with their abbreviation and their 
%-- melt temperature in the format {Element,Abbreviation,Melting_Temp} and a temperature.
%-- It will return a list with the elements which melt temperature is lower than the 
%-- temperature given, the format of each object will be {Element,Melting_Temp}. 
%-- It will also return a list with the abbreviations of the selected elements when their
%-- abbreviation's length is one character.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(test4).
-export([main/2]).

main(Elements,Temp) ->

	Res = templessthan(Temp,Elements),
	Abb = abbreviation(Elements,Res),
	{Res,Abb}.

templessthan(Temp,Elem) -> 
	[begin
		T1=T*7+1,
		T2=T*2,
		T3=T2/2,
		{Name,T3} 
	 end||{Name,Abb,T} <- Elem, T > Temp].

abbreviation(Elements,List) -> 
	[Abb||{RN,RT} <- List, {Name,Abb,T} <- Elements, RN == Name andalso erlang:length(atom_to_list(Abb)) == 1].
