-module(paxos).

-export([start_proposer/2,start_acceptor/0,decide/1]).
-export([test/2,runTests/2,testRemotes/1]).

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
    lists:foreach(fun(_) -> decide(P) end, lists:seq(1,Rounds)),
    ok.

test(F,Rounds) ->
    Acceptors = lists:map(
                  fun(_) -> {ok, A} = start_acceptor(), A end,
                  lists:seq(1,(2 * F) + 1)),
    {ok,P} = start_proposer(F,Acceptors),
    timer:tc(paxos, runTests, [P,Rounds]).

testRemotes(Rounds) ->
    {ok,A1} = rpc:call('a1@localhost', paxos, start_acceptor, []),
    {ok,A2} = rpc:call('a2@localhost', paxos, start_acceptor, []),
    {ok,A3} = rpc:call('a3@localhost', paxos, start_acceptor, []),
    {ok,P} = start_proposer(1,[A1,A2,A3]),
    timer:tc(paxos, runTests, [P,Rounds]).
