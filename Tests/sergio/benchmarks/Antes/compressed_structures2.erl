%ERROR en el siguiente caso:
%---CASO 1---
-module(compressed_structures2).
-export([main/2]).

main(_,ArtistName) ->
	Database = [["Peret",98,1000000,{albums,[{"Rumba pa ti",1999,100},{"Rey de la rumba",2001,10000}]},{concerts,[{"Malaga",{2017,6,12}},{"Huelva",{2016,6,12}}]}],["El Fary",98,1000000,{albums,[{"La Mandanga",1999,100},{"El Fary Vive",2001,10000}]},{concerts,[{"Malaga",{2017,6,12}},{"Huelva",{2016,6,12}}]}]],
	Arti = getFirstArtist(Database),
	Location = getFirstConcertLocation(Arti).

getConcerts(Artist) ->
	[_,_,_,_,{concerts,Concerts}] = Artist,
	Concerts.

%-------------------------------------
getFirstArtist(Db) ->
	[A|_] = Db,
	A.
%-------------------------------------
getFirstConcertLocation(Artist) ->
	Concerts = getConcerts(Artist),
	[{Location,Date}|_] = Concerts,
	Location.
%-------------------------------------
%En este caso, al tomar <8,Location> como criterio de slice la variable Database se coge entera (en algun momento se borra la pila).
%Sabemos por el programa que con tomar "Málaga" como slice sera suficiente por las constraints.
%--------------------------------------------------------------------------
%---CASO 2---
-module(compressed_structures2).
-export([main/2]).

main(_,ArtistName) ->
	Database = [["Peret",98,1000000,{albums,[{"Rumba pa ti",1999,100},{"Rey de la rumba",2001,10000}]},{concerts,["Malaga",{2017,6,12},"Huelva",{2016,6,12}]}],["El Fary",98,1000000,{albums,[{"La Mandanga",1999,100},{"El Fary Vive",2001,10000}]},{concerts,["Malaga",{2017,6,12},"Huelva",{2016,6,12}]}]],
	Arti = getFirstArtist(Database),
	Location = getFirstConcertLocation(Arti).

getConcerts(Artist) ->
	[_,_,_,_,{concerts,Concerts}] = Artist,
	Concerts.

%-------------------------------------
getFirstArtist(Db) ->
	[A|_] = Db,
	A.
%-------------------------------------
getFirstConcertLocation(Artist) ->
	Concerts = getConcerts(Artist),
	[Location,Date|_] = Concerts,
	Location.
%-------------------------------------
%En este segundo caso, tomando el mismo criterio de slice el slicer minimiza la variable Database a solo "Malaga", recorriendo correctamente 
%el grafo con las constraints. Sin embargo, si consideramos <48,Date> como criterio de slice, toma también todo el valor de Database.
%-------------------------------------







