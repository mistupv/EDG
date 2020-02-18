-module(b11).
-export([main/2]).
main(Account,Who) -> 
	{_, Withdraws} = recount(Who, Account).
recount(Person, List) -> 
	tailrecount(Person,List,undef,[]).
tailrecount(_,[],_, LWithdraws) -> 
	{undef,LWithdraws};
tailrecount(Person,[H|T],_,LWithdraws) -> 
	case H of
		{Mov, Amount, Person} when Mov == withdraw andalso Amount >100 -> 
			tailrecount(Person,T,undef,[H|LWithdraws]);
		_ -> 
			tailrecount(Person,T,undef,LWithdraws)
	end.