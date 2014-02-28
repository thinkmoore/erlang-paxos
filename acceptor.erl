-module(acceptor).
-behavior(gen_fsm).

-export([init/1,acceptor/2,terminate/3]).

init(_) ->
    State = case file:consult(node()) of
                {ok, []} ->
                    {0,0,[]};
                {ok, Terms} -> 
                    T = lists:last(Terms),
                    case T of
                        {_,_,_} ->
                            io:fwrite("Acceptor recovering in state ~w~n", [T]),
                            T;
                        _ -> {0,0,[]}
                    end;
                _ -> {0,0,[]}
            end,
    {ok, Log} = file:open(node(),[write]),
    {ok, acceptor, {Log,State}}.

terminate(_,_,{Log,_}) ->
    file:close(Log).

persist(Log,State) ->
    file:write(Log, io_lib:fwrite("~p.~n", [State])),
    file:sync(Log).

acceptor({prepare,Proposer,N}, {Log,{NL,NA,VA}}) ->
    NewState = {max(NL,N),NA,VA},
    persist(Log,NewState),
    gen_fsm:send_event(Proposer,{prepared,NA,VA}),
    {next_state, acceptor, {Log,NewState}};
acceptor({accept,Proposer,N,V}, {Log,{NL,_,_}}) when N >= NL ->
    NewState = {N,N,V},
    persist(Log,NewState),
    gen_fsm:send_event(Proposer,{accepted,N}),
    {next_state, acceptor, {Log,NewState}};
acceptor({accept,Proposer,_,_}, {Log,{NL,NA,VA}}) ->
    gen_fsm:send_event(Proposer,{accepted,NA}),
    {next_state, acceptor, {Log,{NL,NA,VA}}};
acceptor({decided,_}, {Log,{NL,NA,_}}) ->
    NewState = {NL,NA,[]},
    persist(Log,NewState),
    {next_state, acceptor, {Log,NewState}};
acceptor(stop,{Log,{NL,NA,VA}}) ->
    {stop, normal, {Log,{NL,NA,VA}}}.
