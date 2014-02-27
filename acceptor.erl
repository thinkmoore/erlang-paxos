-module(acceptor).
-behavior(gen_fsm).

-export([init/1,acceptor/2]).

init(_) ->
    {ok, acceptor, {0,0,[]}}.

acceptor(Event, {NL,NA,VA}) ->
    case Event of
        {prepare,Proposer,N} ->
            gen_fsm:send_event(Proposer,{prepared,NA,VA}),
            {next_state, acceptor, {max(NL,N),NA,VA}};
        {accept,Proposer,N,V} ->
            {NewNL,NewNA,NewVA} = if
                                N >= NL ->
                                    {N,N,V};
                                true ->
                                    {NL,NA,VA}
                            end,
            gen_fsm:send_event(Proposer,{accepted,NewNA}),
            {next_state, acceptor, {NewNL,NewNA,NewVA}};
        {decided,V} ->
            {next_state, acceptor, {NL,NA,[]}};
        stop ->
            {stop, normal, {NL,NA,VA}}
    end.
