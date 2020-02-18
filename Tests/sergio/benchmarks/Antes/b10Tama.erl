-module(b10Tama).
-export([main/2]).
main(Account, Who) ->
    {Deposits,_} = recount(Who, Account).
recount(Person, List) ->
    tailrecount(Person, List, [], []).
tailrecount(_, [], LDeposits, LWithdraws) ->
    {LDeposits,LWithdraws};
tailrecount(Person, [H|T], LDeposits, LWithdraws) ->
    case H of
        {Mov,Amount,Person}
            when
                Mov == withdraw
                andalso
                Amount > 100 ->
            tailrecount(Person, T, LDeposits, undef);
        {Mov,Amount,Person}
            when
                Mov == deposit
                andalso
                Amount > 300 ->
            tailrecount(Person, T, [H|LDeposits], LWithdraws);
        _ ->
            tailrecount(Person, T, LDeposits, LWithdraws)
    end.
