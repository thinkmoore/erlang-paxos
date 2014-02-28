-module(acceptor).
-behavior(gen_fsm).

-export([init/1,acceptor/2]).

init(_) ->
    {ok, acceptor, {0,0,[]}}.

acceptor({prepare,Proposer,N}, {NL,NA,VA}) ->
    gen_fsm:send_event(Proposer,{prepared,NA,VA}),
    {next_state, acceptor, {max(NL,N),NA,VA}};
acceptor({accept,Proposer,N,V}, {NL,_,_}) when N >= NL ->
    gen_fsm:send_event(Proposer,{accepted,N}),
    {next_state, acceptor, {N,N,V}};
acceptor({accept,Proposer,_,_}, {NL,NA,VA}) ->
    gen_fsm:send_event(Proposer,{accepted,NA}),
    {next_state, acceptor, {NL,NA,VA}};
acceptor({decided,_}, {NL,NA,_}) ->
    {next_state, acceptor, {NL,NA,[]}};
acceptor(stop,{NL,NA,VA}) ->
    {stop, normal, {NL,NA,VA}}.
