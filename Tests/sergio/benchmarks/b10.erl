%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench8.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test tail recursion and guards.
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The program receives a list of movements in a saving account with the format 
%-- {Movement, Amount, User} and a user Who. It returns two lists, one list
%-- for user's deposits greater than 300 and another one for the withdraws 
%-- greater than 100.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b10).
-export([main/2]).

main(Account,Who) -> 
	{Deposits, Withdraws} = recount(Who, Account).
recount(Person, List) -> 
	tailrecount(Person,List,[],[]).
tailrecount(Person,[],LDeposits, LWithdraws) -> 
	{LDeposits,LWithdraws};
tailrecount(Person,[H|T],LDeposits,LWithdraws) -> 
	case H of
		{Mov, Amount, Person} when Mov == withdraw andalso Amount >100 -> 
			tailrecount(Person,T,LDeposits,[H|LWithdraws]);
		{Mov, Amount, Person} when Mov == deposit andalso Amount >300 ->
			tailrecount(Person,T,[H|LDeposits],LWithdraws);
		_ -> 
			tailrecount(Person,T,LDeposits,LWithdraws)
	end.