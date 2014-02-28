-module(paxos).

-export([start_proposer/2,start_acceptor/0,decide/1]).
-export([test/1,runTests/2]).

start_proposer(F,Acceptors) ->
    gen_fsm:start_link(proposer, [F|Acceptors], []).

start_acceptor() ->
    gen_fsm:start_link(acceptor, [], []).

decide(Proposer) ->
    gen_fsm:send_all_state_event(Proposer,{propose,self()}),
    receive
        stopped ->
            none;
        {decided,V} ->
            V
    after
        1000 -> 
            decide(Proposer)
    end.

runTests(P,Rounds) ->
    lists:foreach(fun(_) -> 
                          V = decide(P),
                          io:fwrite("Decided on ~w~n", [V])
                  end,
                  lists:seq(1,Rounds)),
    ok.

test(Rounds) ->
    {Responses,_} = rpc:multicall(paxos,start_acceptor,[]),
    Acceptors = lists:map(fun({ok,A}) -> A end, Responses),
    Count = length(Acceptors),
    F = (Count div 2) + 1,
    io:fwrite("Starting paxos test with ~w acceptors, F = ~w~n", [Count,F]),
    {ok,P} = start_proposer(F,Acceptors),
    Out = timer:tc(paxos, runTests, [P,Rounds]),
    gen_fsm:send_all_state_event(P,stop),
    Out.
