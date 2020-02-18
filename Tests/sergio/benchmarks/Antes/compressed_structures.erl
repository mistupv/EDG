-module(compressed_structures).
-export([main/1]).

main(Number) ->
	Database = [
	["Rihanna",28,
	{albums,[{"Music of the Sun",2005},{"A Girl like me",2006},{"Good Girl Gone Bad",2007},{"Rater D",2009},{"Loud",2010},{"Talk That Talk",2011},{"Unapologetic",2012},{"ANTi",2016}]},
	{concerts,[{"Amsterdam",{2016,6,17}}, {"Manchester",{2016,6,29}}, {"Barcelona",{2016,7,21}}, {"Bucarest",{2016,8,14}}]}],
	["Christina Aguilera",35,
	{albums,[{"Christina Aguilera",1999},{"Mi Reflejo",2000},{"Stripped",2002},{"Back to Basics",2006},{"Bionic",2010},{"Lotus",2012}],
	{concerts,[{"Tokio",{2007,6,21}},{"Abu Dabi",{2008,10,28}}]}}],
	["Bruno Mars",30,
	{albums,[{"Doo-Wops & Hooligans",2010},{"Unorthodox Jukebox",2012}]},
	{concerts,[{"Santo Domingo",{2014,10,4}},{"Las Vegas",{2014,10,18}},{"Liverpool",{2013,11,24}}]}],
	["Daddy Yankee",39,
	{albums,[{"No Mercy",1995},{"El Cangri.Com",2002},{"Barrio Fino",2004},{"El Cartel: The Big Boss",2007},{"Mundial",2010},{"Prestige",2012},{"Cartel IV",2015}]},
	{concerts,[{"Mexico City",{2015,11,8}},{"Las Vegas",{2016,5,6}},{"New York",{2016,7,30}}]}],
	["Justin Bieber",22,
	{albums,[{"My World 2.0",2010},{"Under the mistletoe",2011},{"Believe",2012},{"Purpose",2015}]},
	{concerts,[{"Miami",{2016,7,3}},{"Munich",{2016,9,16}},{"Birmingham",{2016,10,24}}]}],
	["Adele",28,
	{albums,[{"19",2008},{"21",2011},{"25",2015}]},
	{concerts,[{"Lisboa",{2016,5,22}},{"Paris",{2016,6,10}},{"Oakland",{2016,8,2}},{"Toronto",{2016,10,6}}]}]
	],

	Artist = lists:nth((Number rem 6)+1,Database),
	ArtistName = getArtistName(Artist),
	Age = getAge(Artist),
	LastDisk = getLastDisk(Artist),
	NextConcert = getNextConcert(Artist),
	FirstArtist = getFirstArtist(Database),
	Location = getFirstConcertLocation(FirstArtist),
	FirstAlbumName = getFirstAlbumName(FirstArtist),
	io:format("The artist ~s is ~p years old.\nHis last album is ~p and his next concert is in ~s (~s).\n",
		[ArtistName,Age,getAlbumName(LastDisk),getConcertLocation(NextConcert),getStringDate(NextConcert)]),
	io:format("The first artist in the data base is ~s.\nHer first concert known was/will be in ~s,\nand her first album composed was ~p.\n",
		[getArtistName(FirstArtist),Location,FirstAlbumName]).

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

getLastDisk(Artist) ->
	Albums = getDiscography(Artist),
	case getLast(Albums,undef) of 
		undef -> "No albums published";
		Album -> Album
	end.

getLast([],A) -> A;
getLast([Album|Albums],undef) ->
	getLast(Albums,Album);
getLast([Album|Albums],Newest) ->
	{_,YearN} = Newest,
	{_,YearA} = Album,
	case YearA > YearN of
		true -> getLast(Albums,Album);
		_ -> getLast(Albums,Newest)
	end.

getNextConcert(Artist) ->
	ActualDate = erlang:date(),
	Concerts = getConcerts(Artist),
	case getNext(Concerts,ActualDate,undef) of
		undef -> "No concerts planned";
		Concert -> Concert
	end.

getNext([],_,NextConcert) -> NextConcert;
getNext([Concert|Concerts],Actual,undef) ->
	{YearA,MonthA,DayA} = Actual,
	{_,Date}=Concert,
	{YearC,MonthC,DayC} = Date,
	case {YearA,YearC,MonthA,MonthC,DayA,DayC} of
		{YA,YC,_,_,_,_} when YC > YA -> getNext(Concerts,Actual,Concert);
		{Y,Y,MA,MC,_,_} when MC > MA -> getNext(Concerts,Actual,Concert);
		{Y,Y,M,M,DA,DC} when DC >= DA -> getNext(Concerts,Actual,Concert);
		_ -> getNext(Concerts,Actual,undef)
	end;
getNext([Concert|Concerts],Actual,NextConcert) ->
	{YearA,MonthA,DayA} = Actual,
	{_,DateN}=NextConcert,
	{YearN,MonthN,DayN} = DateN,
	{_,DateC}=Concert,
	{YearC,MonthC,DayC} = DateC,

	Next = case {YearA,YearC,MonthA,MonthC,DayA,DayC} of
		{YA,YC,_,_,_,_} when YC < YA -> getNext(Concerts,Actual,NextConcert);
		{Y,Y,MA,MC,_,_} when MC < MA -> getNext(Concerts,Actual,NextConcert);
		{Y,Y,M,M,DA,DC} when DC < DA -> getNext(Concerts,Actual,NextConcert);
		_ -> undef
	end,
	case Next of
		undef ->
			case {YearN,YearC,MonthN,MonthC,DayN,DayC} of
				{YN,YC2,_,_,_,_} when YC2 < YN -> getNext(Concerts,Actual,Concert);
				{Y2,Y2,MN,MC2,_,_} when MC2 < MN -> getNext(Concerts,Actual,Concert);
				{Y2,Y2,M2,M2,DN,DC2} when DC2 < DN -> getNext(Concerts,Actual,Concert);
				_ -> getNext(Concerts,Actual,NextConcert)
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

getConcertLocation(Concert) ->
	{Location,_} = Concert,
	Location.

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
getFirstAlbumName(Artist) ->
	[_,_,{albums,Discography},_] = Artist,
	[Album|_] = Discography,
	{AlName,_} = Album,
	AlName.
%-------------------------------------
