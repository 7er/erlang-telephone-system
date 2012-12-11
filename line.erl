-module(line).
-behaviour(gen_fsm).

-export([start/1, off_hook/1, on_hook/1, dial/2, media/2, attach/1]).
-export([idle/3, wait_for_attach/3, dial_tone/3, ringing/3, calling/2, calling/3, 
         speech/3, wait_on_hook/3]).
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).


start(LineNumberAtom) ->
    gen_fsm:start({local, LineNumberAtom}, line, [], []).

attach(LineNumber) ->
    gen_fsm:sync_send_event(LineNumber, attach).

off_hook(LineNumber) -> gen_fsm:sync_send_event(LineNumber, off_hook).

on_hook(LineNumber) -> gen_fsm:sync_send_event(LineNumber, on_hook).

dial(LineNumber, OtherLine) when is_atom(OtherLine) ->
    dial(LineNumber, whereis(OtherLine));
dial(LineNumber, OtherLinePid) when is_pid(OtherLinePid) ->
    gen_fsm:sync_send_event(LineNumber, {dial, OtherLinePid}).

media(LineNumber, Message) -> 
    LineNumber ! {media, self(), Message}.

init(_) ->
    {ok, wait_for_attach, []}.


% internal event functions
connect(OtherLine) -> gen_fsm:sync_send_event(OtherLine, connect).
disconnect(OtherLine) -> gen_fsm:sync_send_event(OtherLine, disconnect).
answer(OtherLine) -> gen_fsm:send_event(OtherLine, answer).
     



wait_for_attach(attach, {Phone, _}, []) ->
    {reply, ok, idle, Phone};
wait_for_attach(_, _, []) ->
    {reply, error, wait_for_attach, []}.

idle(off_hook, {Phone, _}, Phone) ->
    {reply, ok, dial_tone, Phone};
idle(connect, {ConnectingLine, _}, Phone) ->
    {reply, ok, ringing, {Phone, ConnectingLine}};
idle(_, _, Phone) ->
    {reply, error, idle, Phone}.

dial_tone(on_hook, {Phone, _}, Phone) ->
    {reply, ok, idle, Phone};
dial_tone({dial, OtherLine}, {Phone, _}, Phone) ->
    ok = connect(OtherLine),
    {reply, ok, calling, {Phone, OtherLine}};
dial_tone(_, _, Phone) ->
    {reply, error, dial_tone, Phone}.


ringing(disconnect, {ConnectingLine, _}, {Phone, ConnectingLine}) ->
    {reply, ok, idle, Phone};
ringing(off_hook, {Phone, _}, {Phone, ConnectingLine}) ->
    answer(ConnectingLine),
    {reply, ok, speech, {Phone, ConnectingLine, incoming}}.
            

calling(on_hook, {Phone, _}, {Phone, OtherLine}) ->
    ok = disconnect(OtherLine),
    {reply, ok, idle, Phone}.

calling(answer, {Phone, OtherLine}) ->
    {next_state, speech, {Phone, OtherLine, outgoing}}.


speech(on_hook, {Phone, _}, {Phone, OtherLine, outgoing}) ->
    ok = disconnect(OtherLine),
    {reply, ok, idle, Phone};
speech(disconnect, {ConnectingLine, _}, {Phone, ConnectingLine, incoming}) ->
    {reply, ok, wait_on_hook, Phone}.


handle_info({media, Phone, Content}, speech, {Phone, OtherLine, outgoing}=State) ->
    media(OtherLine, Content),
    {next_state, speech, State};
handle_info({media, ConnectingLine, Content}, speech, {Phone, ConnectingLine, incoming}=State) ->
    media(Phone, Content),
    {next_state, speech, State}.

wait_on_hook(on_hook, {Phone, _}, Phone) ->
    {reply, ok, idle, Phone}.    
    
terminate(normal, _, _) ->        
    ok.
            

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) -> 
    {reply, error, StateName, StateData}.
