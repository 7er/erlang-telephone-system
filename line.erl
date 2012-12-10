-module(line).
-export([start/1, off_hook/1, on_hook/1, dial/2, media/2, attach/1]).
-export([init/0]).

% API 
start(LineNumberAtom) ->
    register(LineNumberAtom, spawn(line, init, [])).

attach(LineNumber) ->
    call(LineNumber, attach).

off_hook(LineNumber) -> call(LineNumber, off_hook).

on_hook(LineNumber) -> call(LineNumber, on_hook).

dial(LineNumber, OtherLine) when is_atom(OtherLine) ->
    dial(LineNumber, whereis(OtherLine));
dial(LineNumber, OtherLinePid) when is_pid(OtherLinePid) ->
    call(LineNumber, {dial, OtherLinePid}).

media(LineNumber, Message) -> 
    LineNumber ! {media, self(), Message}.

call(RegisteredProcess, Message) when is_atom(RegisteredProcess) ->
    call(whereis(RegisteredProcess), Message);
call(Pid, Message) when is_pid(Pid) ->
    Pid ! {request, self(), Message},
    receive
        {reply, Pid, Reply} -> Reply
    end.


% internals
reply(Pid, Message) ->
    Pid ! {reply, self(), Message}.


init() ->
    loop_wait_for_attach().

connect(OtherLine) -> call(OtherLine, connect).
disconnect(OtherLine) -> call(OtherLine, disconnect).
    
    
loop_wait_for_attach() ->
    receive 
        {request, Phone, attach} ->
            reply(Phone, ok),
            loop_idle(Phone);
         {request, Pid, _} ->
            reply(Pid, error),
            loop_wait_for_attach()
    end.

loop_idle(Phone) ->
    receive 
        {request, Phone, off_hook} ->
            reply(Phone, ok),
            loop_dial_tone(Phone);
        {request, ConnectingLine, connect} ->
            reply(ConnectingLine, ok),
            loop_ringing(Phone, ConnectingLine);
        {request, Pid, _} -> 
            reply(Pid, error),
            loop_idle(Phone)
    end.

loop_dial_tone(Phone) ->
    receive
        {request, Phone, on_hook} ->
            reply(Phone, ok),
            loop_idle(Phone);
        {request, Phone, {dial, OtherLine}} ->
            reply(Phone, ok),
            ok = connect(OtherLine),
            loop_calling(Phone, OtherLine);
        {request, Pid, _} -> 
            reply(Pid, error),
            loop_dial_tone(Phone)
    end.

loop_wait_on_hook(Phone) ->
    receive
        {request, Phone, on_hook} ->
            reply(Phone, ok),
            loop_idle(Phone);
        {request, Pid, _} -> 
            reply(Pid, error),
            loop_wait_on_hook(Phone)
    end.        

loop_ringing(Phone, ConnectingLine) ->
    receive 
        {request, ConnectingLine, disconnect} ->
            reply(ConnectingLine, ok),
            loop_idle(Phone);
         {request, Phone, off_hook} ->
            reply(Phone, ok),
            ConnectingLine ! {cast, self(), answer},
            loop_speech(Phone, ConnectingLine, incoming);
        {request, Pid, _} ->
            reply(Pid, error),
            loop_ringing(Phone, ConnectingLine)
    end.
            

loop_calling(Phone, OtherLine) ->
    receive                                           
        {request, Phone, on_hook} ->
            ok = disconnect(OtherLine),
            reply(Phone, ok),
            loop_idle(Phone);
        {cast, OtherLine, answer} ->            
            loop_speech(Phone, OtherLine, outgoing);
        {request, Pid, _} ->
            reply(Pid, error),
            loop_calling(Phone, OtherLine)
    end.

loop_speech(Phone, OtherLine, outgoing=Direction) ->
    receive
        {request, Phone, on_hook} ->
            ok = disconnect(OtherLine),
            reply(Phone, ok),
            loop_idle(Phone);
        {request, Pid, _} ->
            reply(Pid, error),
            loop_speech(Phone, OtherLine, Direction);
        Anything ->
            OtherLine ! Anything,
            loop_speech(Phone, OtherLine, Direction)
    end;
loop_speech(Phone, ConnectingLine, incoming=Direction) ->
    receive 
        {request, ConnectingLine, disconnect} ->
            reply(ConnectingLine, ok),
            loop_wait_on_hook(Phone);
        {request, Pid, _} ->
            reply(Pid, error),
            loop_speech(Phone, ConnectingLine, Direction);
        {media, Pid, Content} ->
            Phone ! {media, self(), Content},
            loop_speech(Phone, ConnectingLine, Direction)
    end.


        
            

