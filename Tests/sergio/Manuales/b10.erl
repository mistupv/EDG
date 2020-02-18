-module(b10).
-export([main/2]).
main(Account,Who) -> 
	{Deposits, _} = recount(Who, Account).
recount(Person, List) -> 
	tailrecount(Person,List,[],undef).
tailrecount(_,[],LDeposits, _) -> 
	{LDeposits,undef};
tailrecount(Person,[H|T],LDeposits,_) -> 
	case H of
		{Mov, Amount, Person} when Mov == deposit andalso Amount >300 ->
			tailrecount(Person,T,[H|LDeposits],undef);
		_ -> 
			tailrecount(Person,T,LDeposits,undef)
	end.