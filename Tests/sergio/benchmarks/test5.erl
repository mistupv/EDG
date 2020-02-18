%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test5.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test tail recursion and guards.
%-- DESCRIPTION
%-- The program reveives a list of movements in a savings account with the format 
%-- {Kind_of_Movement, Amount, User} and a user Who. It will return two lists, one list
%-- for the deposits of this user greater than 300 and another one for the withdraws 
%-- greater than 100.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(test5).
-export([main/2]).

main(Account,Who) -> 
	
	{Deposits, Withdraws} = recount(Who, Account).

recount(Person, List) -> tailrecount(Person,List,[],[]).

tailrecount(Person,[],LDeposits, LWithdraws) -> {LDeposits,LWithdraws};
tailrecount(Person,[H|T],LDeposits,LWithdraws) -> 
	case H of
		{Mov, Amount, Person} when Mov == withdraw andalso Amount >100 -> 
			tailrecount(Person,T,LDeposits,[H|LWithdraws]);
		{Mov, Amount, Person} when Mov == deposit andalso Amount >300 ->
			tailrecount(Person,T,[H|LDeposits],LWithdraws);
		_ -> tailrecount(Person,T,LDeposits,LWithdraws)
	end.
