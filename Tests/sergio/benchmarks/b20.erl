%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench1.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test compression and expansion of
%-- 			 complex structures.
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- This benchmark consists in a program with a database of singers. It returns a set
%-- of information of each singer (Name,Age,Last Album Name, and Location and Year of 
%-- their next concert) by receiving a number between 1 and 6 as input.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b20).
-export([main/1]).

main(Number) when Number > 0 andalso Number < 7->
	Database = [
	["Rihanna",28,
	{albums,[{"Music of the Sun",2005},{"A Girl like me",2006},{"Good Girl Gone Bad",2007},{"Rater D",2009},{"Loud",2010},{"Talk That Talk",2011},{"Unapologetic",2012},{"ANTi",2016}]},
	{concerts,[{"Amsterdam",{2016,6,17}}, {"Manchester",{2016,6,29}}, {"Barcelona",{2016,7,21}}, {"Bucarest",{2017,8,14}}]}],
	["Christina Aguilera",35,
	{albums,[{"Christina Aguilera",1999},{"Mi Reflejo",2000},{"Stripped",2002},{"Back to Basics",2006},{"Bionic",2010},{"Lotus",2012}]},
	{concerts,[{"Tokio",{2007,6,21}},{"Abu Dabi",{2008,10,28}}]}],
	["Bruno Mars",30,
	{albums,[{"Doo-Wops & Hooligans",2010},{"Unorthodox Jukebox",2012}]},
	{concerts,[{"Santo Domingo",{2014,10,4}},{"Las Vegas",{2014,10,18}},{"Liverpool",{2013,11,24}}]}],
	["Daddy Yankee",39,
	{albums,[{"No Mercy",1995},{"El Cangri.Com",2002},{"Barrio Fino",2004},{"El Cartel: The Big Boss",2007},{"Mundial",2010},{"Prestige",2012},{"Cartel IV",2015}]},
	{concerts,[{"Mexico City",{2015,11,8}},{"Las Vegas",{2016,5,6}},{"New York",{2017,7,30}}]}],
	["Justin Bieber",22,
	{albums,[{"My World 2.0",2010},{"Under the mistletoe",2011},{"Believe",2012},{"Purpose",2015}]},
	{concerts,[{"Miami",{2016,7,3}},{"Munich",{2016,9,16}},{"Birmingham",{2017,10,24}}]}],
	["Adele",28,
	{albums,[{"19",2008},{"21",2011},{"25",2015}]},
	{concerts,[{"Lisboa",{2016,5,22}},{"Paris",{2016,6,10}},{"Oakland",{2016,8,2}},{"Toronto",{2017,10,6}}]}]
	],
	Artist = lists:nth(Number,Database),	
	ArtistName = getArtistName(Artist),	
	Age = getAge(Artist),
	LastAlbum = getLastAlbum(Artist),
	AlbumName = getAlbumName(LastAlbum),
	NextConcert = getNextConcert(Artist),
	Info = getConcertLocationAndYear(NextConcert),
	{Location,Year} = Info,
	{ArtistName,Age,AlbumName,Location,Year}.

getArtistName(Artist) ->
	[Name|_] = Artist,
	Name.

getAge(Artist) ->
	[_,Age|_] = Artist,
	Age.

getDiscography(Artist) ->
	[_,_,{albums,Discography}|_] = Artist,
	Discography.

getConcerts(Artist) ->
	[_,_,_,{concerts,Concerts}] = Artist,
	Concerts.

getLastAlbum(Artist) ->
	Albums = getDiscography(Artist),
	case getLast(Albums,empty) of 
		empty -> "No albums published";
		Album -> Album
	end.

getLast([],A) -> A;
getLast([Album|Albums],empty) ->
	getLast(Albums,Album);
getLast([Album|Albums],Newest) ->
	{_,YearN} = Newest,
	{_,YearA} = Album,
	case YearA > YearN of
		true -> getLast(Albums,Album);
		_ -> getLast(Albums,Newest)
	end.

getNextConcert(Artist) ->
	CurrentDate = {2017,3,28},
	Concerts = getConcerts(Artist),
	case getNext(Concerts,CurrentDate,empty) of
		empty -> "No future concerts planned";
		Concert -> Concert
	end.

getNext([],_,NextConcert) -> NextConcert;
getNext([Concert|Concerts],Current,empty) ->
	{YearA,MonthA,DayA} = Current,
	{_,Date}=Concert,
	{YearC,MonthC,DayC} = Date,
	case {YearA,YearC,MonthA,MonthC,DayA,DayC} of
		{YA,YC,_,_,_,_} when YC > YA -> getNext(Concerts,Current,Concert);
		{Y,Y,MA,MC,_,_} when MC > MA -> getNext(Concerts,Current,Concert);
		{Y,Y,M,M,DA,DC} when DC >= DA -> getNext(Concerts,Current,Concert);
		_ -> getNext(Concerts,Current,empty)
	end;
getNext([Concert|Concerts],Current,NextConcert) ->
	{YearA,MonthA,DayA} = Current,
	{_,DateN}=NextConcert,
	{YearN,MonthN,DayN} = DateN,
	{_,DateC}=Concert,
	{YearC,MonthC,DayC} = DateC,

	Next = case {YearA,YearC,MonthA,MonthC,DayA,DayC} of
		{YA,YC,_,_,_,_} when YC < YA -> getNext(Concerts,Current,NextConcert);
		{Y,Y,MA,MC,_,_} when MC < MA -> getNext(Concerts,Current,NextConcert);
		{Y,Y,M,M,DA,DC} when DC < DA -> getNext(Concerts,Current,NextConcert);
		_ -> empty
	end,
	case Next of
		empty ->
			case {YearN,YearC,MonthN,MonthC,DayN,DayC} of
				{YN,YC2,_,_,_,_} when YC2 < YN -> getNext(Concerts,Current,Concert);
				{Y2,Y2,MN,MC2,_,_} when MC2 < MN -> getNext(Concerts,Current,Concert);
				{Y2,Y2,M2,M2,DN,DC2} when DC2 < DN -> getNext(Concerts,Current,Concert);
				_ -> getNext(Concerts,Current,NextConcert)
			end;
		_ -> Next
	end.

getAlbumName(Album) -> 
	{Name,_} = Album,
	Name.

getStringDate(Concert) ->
	{_,Date} = Concert,
	{Y,M,D} = Date,
	integer_to_list(D)++"/"++integer_to_list(M)++"/"++integer_to_list(Y).

getConcertLocationAndYear("No future concerts planned") ->
	{"an undefined City", "Not planned yet"};
getConcertLocationAndYear(Concert) ->
	{Location,Date} = Concert,
	{Year,Month,Day} = Date,
	Info = {Location,Year},
	Info.
