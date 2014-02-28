-module(proposer).
-behavior(gen_fsm).

-export([init/1,handle_event/3,ready/2,awaitPrepared/2,awaitAccepted/2]).

init([F|Acceptors]) ->
    {ok, ready, {F,Acceptors,0,0,0,[],[]}}.

broadcast(Acceptors,Message) ->
    lists:map(
      fun(Acceptor) ->
              gen_fsm:send_event(Acceptor,Message)
      end,
      Acceptors).

handle_event({propose,From},_,{F,Acceptors,NP,_,_,_,_}) ->
    NewNP = NP + 1,
    broadcast(Acceptors,{prepare,self(),NewNP}),
    {next_state, awaitPrepared, {F,Acceptors,NP,0,0,[],From}};
handle_event(stop,_,{F,Acceptors,NP,A,NO,VO,Notify}) ->
    Notify ! stopped,
    {stop, normal, {F,Acceptors,NP,A,NO,VO,[]}}.

ready(_,State) ->
    {next_state, ready, State}.

awaitPrepared({prepared,N,V}, {F,Acceptors,NP,A,NO,VO,Notify}) when A == F ->
    {NewNO,NewVO} = if N > NO -> {N,V};
                       true -> {NO,VO}
                    end,
    Chosen = case NewVO of
                 [] -> random:uniform(100);
                 _ -> NewVO
             end,
    NewNP = max(NP,NewNO),
    broadcast(Acceptors,{accept,self(),NewNP,Chosen}),
    {next_state, awaitAccepted, {F,Acceptors,NewNP,0,NewNO,Chosen,Notify}};
awaitPrepared({prepared,N,V}, {F,Acceptors,NP,A,NO,VO,Notify}) ->
    {NewNO,NewVO} = if N > NO -> {N,V};
                       true -> {NO,VO}
                    end,
    NewA = A + 1,
    {next_state, awaitPrepared, {F,Acceptors,NP,NewA,NewNO,NewVO,Notify}};
awaitPrepared({accepted,_},State) ->
    {next_state, awaitPrepared, State}.

awaitAccepted({accepted,_},{F,Acceptors,NP,A,_,VO,Notify}) when A == F ->
    broadcast(Acceptors,{decided,VO}),
    Notify ! {decided,VO},
    {next_state, ready, {F,Acceptors,NP,0,0,[],[]}};
awaitAccepted({accepted,N},{F,Acceptors,NP,A,NO,VO,Notify}) ->
    if
        N == NP ->
            {next_state, awaitAccepted, {F,Acceptors,NP,A+1,NO,VO,Notify}};
        true  ->
            {next_state, awaitAccepted, {F,Acceptors,NP,A,NO,VO,Notify}}
    end;
awaitAccepted({prepared,_,_},State) ->
    {next_state, awaitAccepted, State}.
