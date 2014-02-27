-module(proposer).
-behavior(gen_fsm).

-export([init/1,proposer/2]).

init([F|Acceptors]) ->
    {ok, proposer, {F,Acceptors,0,0,0,0,[],[]}}.

broadcast(Acceptors,Message) ->
    lists:map(
      fun(Acceptor) ->
              gen_fsm:send_event(Acceptor,Message)
      end,
      Acceptors).

notify(Waiting,Message) ->
    lists:map(fun(P) -> P ! Message end, Waiting).

proposer(Event, {F,Acceptors,NP,P,A,NO,VO,Notify}) ->
    case Event of
        {propose,From} ->
            NewNP = NP + 1,
            broadcast(Acceptors,{prepare,self(),NewNP}),
            {next_state, proposer, {F,Acceptors,NewNP,0,0,0,[],[From|Notify]}};
        {prepared,N,V} ->
            {NewNO,NewVO} = if
                          N > NO ->
                              {N,V};
                          true ->
                              {NO,VO}
                      end,
            NewP = P + 1,
            if
                NewP == F + 1 ->
                    Chosen = case VO of
                                 [] -> random:uniform(100);
                                 _ -> VO
                             end,
                    NewNP = max(NP,NewNO),
                    broadcast(Acceptors,{accept,self(),NewNP,Chosen}),
                    {next_state, proposer, {F,Acceptors,NewNP,NewP,A,NewNO,Chosen,Notify}};
                true ->
                    {next_state, proposer, {F,Acceptors,NP,NewP,A,NewNO,NewVO,Notify}}
            end;
        {accepted,N} ->
            NewA = if 
                    N == NP -> A + 1;
                    true -> A
                end,
            if
                NewA == F + 1 ->
                    broadcast(Acceptors,{decided,VO}),
                    notify(Notify,{decided,VO}),
                    {next_state, proposer, {F,Acceptors,NP,P,NewA,NO,VO,[]}};
                true ->
                    {next_state, proposer, {F,Acceptors,NP,P,NewA,NO,VO,Notify}}
            end;
        stop ->
            broadcast(Notify,stopped),
            {stop, normal, {F,Acceptors,NP,P,A,NO,VO,[]}}
    end.
