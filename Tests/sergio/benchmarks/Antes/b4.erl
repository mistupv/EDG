%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench4.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test list comprehensions and blocks. 
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The program receives a list of chemical elements with their abbreviation and their 
%-- melt temperature in the format {Element,Abbreviation,Melting_Temp} and a temperature.
%-- It returns a list with the elements whose melt temperature is greater than the 
%-- given temperature, the format of each object is {Element,Melting_Temp}. 
%-- It also returns a list with the abbreviations of the selected elements when their
%-- length is one character.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b4).
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
	 end||
	 	{Name,Abb,T} <- Elem, 
	 	T > Temp].

abbreviation(Elements,List) -> 
	[Abb||
		{RN,RT} <- List, 
		{Name,Abb,T} <- Elements,
		 RN == Name andalso erlang:length(atom_to_list(Abb)) == 1].
