-module(proposer).
-behavior(gen_fsm).

-export([init/1,handle_event/3,ready/2,awaitPrepared/2,awaitAccepted/2,terminate/3]).

init([F|Acceptors]) ->
    NP = case file:consult(node()) of
             {ok, []} -> 0;
             {ok, Terms} ->
                 T = lists:last(Terms),
                 io:fwrite("Proposer recovering in state ~w~n", [T]),
                 T;
             _ -> 0
            end,
    {ok, Log} = file:open(node(),[write]),
    {ok, ready, {Log,F,Acceptors,NP,0,0,[],[]}}.

terminate(_,_,{Log,_,Acceptors,_,_,_,_,_}) ->
    file:close(Log),
    broadcast(Acceptors,stop).

persist(Log,State) ->
    file:write(Log, io_lib:fwrite("~p.~n", [State])),
    file:sync(Log).

broadcast(Acceptors,Message) ->
    lists:map(
      fun(Acceptor) ->
              gen_fsm:send_event(Acceptor,Message)
      end,
      Acceptors).

handle_event({propose,From},_,{Log,F,Acceptors,NP,_,_,_,_}) ->
    NewNP = NP + 1,
    persist(Log,NewNP),
    broadcast(Acceptors,{prepare,self(),NewNP}),
    {next_state, awaitPrepared, {Log,F,Acceptors,NewNP,0,0,[],From}};
handle_event(stop,_,{Log,F,Acceptors,NP,A,NO,VO,[]}) ->
    {stop, normal, {Log,F,Acceptors,NP,A,NO,VO,[]}};
handle_event(stop,_,{Log,F,Acceptors,NP,A,NO,VO,Notify}) ->
    Notify ! stopped,
    {stop, normal, {Log,F,Acceptors,NP,A,NO,VO,[]}}.

ready(_,State) ->
    {next_state, ready, State}.

awaitPrepared({prepared,N,V}, {Log,F,Acceptors,NP,A,NO,VO,Notify}) when A == F ->
    {NewNO,NewVO} = if N > NO -> {N,V};
                       true -> {NO,VO}
                    end,
    Chosen = case NewVO of
                 [] -> random:uniform(100);
                 _ -> NewVO
             end,
    NewNP = max(NP,NewNO),
    persist(Log,NewNP),
    broadcast(Acceptors,{accept,self(),NewNP,Chosen}),
    {next_state, awaitAccepted, {Log,F,Acceptors,NewNP,0,NewNO,Chosen,Notify}};
awaitPrepared({prepared,N,V}, {Log,F,Acceptors,NP,A,NO,VO,Notify}) ->
    {NewNO,NewVO} = if N > NO -> {N,V};
                       true -> {NO,VO}
                    end,
    NewA = A + 1,
    {next_state, awaitPrepared, {Log,F,Acceptors,NP,NewA,NewNO,NewVO,Notify}};
awaitPrepared({accepted,_},State) ->
    {next_state, awaitPrepared, State}.

awaitAccepted({accepted,_},{Log,F,Acceptors,NP,A,_,VO,Notify}) when A == F ->
    broadcast(Acceptors,{decided,VO}),
    Notify ! {decided,VO},
    {next_state, ready, {Log,F,Acceptors,NP,0,0,[],[]}};
awaitAccepted({accepted,N},{Log,F,Acceptors,NP,A,NO,VO,Notify}) ->
    if
        N == NP ->
            {next_state, awaitAccepted, {Log,F,Acceptors,NP,A+1,NO,VO,Notify}};
        true  ->
            {next_state, awaitAccepted, {Log,F,Acceptors,NP,A,NO,VO,Notify}}
    end;
awaitAccepted({prepared,_,_},State) ->
    {next_state, awaitAccepted, State}.
