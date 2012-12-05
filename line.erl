-module(line).
-export([start/1, acquire/1, release/1, connect/2, media/2]).
-export([init/0]).

% API 
start(LineNumberAtom) ->
    register(LineNumberAtom, spawn(line, init, [])).

attach(LineNumber, Phone) ->
    call(LineNumber, {attach, LineNumber}).

acquire(LineNumber) -> call(LineNumber, acquire).

release(LineNumber) -> call(LineNumber, release).

connect(LineNumber, OtherLine) ->
    call(LineNumber, {connect, OtherLine}).

media(LineNumber, Message) -> 
    LineNumber ! {media, self(), Message}.


call(LineNumber, Message) ->
    Pid = whereis(LineNumber),
    Pid ! {request, self(), Message},
    receive
        {reply, Pid, Reply} -> Reply
    end.


% internals
init() ->
    loop_released().

reply(Pid, Message) ->
    Pid ! {reply, self(), Message}.



loop_released() ->
    receive 
        {request, User, acquire} ->
            reply(User, ok),
            loop_acquired(User);
        {request, Pid, _} -> 
            reply(Pid, error),
            loop_released()
    end.

loop_acquired(User) ->
    receive
        {request, User, release} ->
            reply(User, ok),
            loop_released();
        {request, User, {connect, OtherLine}} ->
            ok = acquire(OtherLine),
            reply(User, ok),
            loop_connected(User, OtherLine);
        {request, Pid, _} -> 
            reply(Pid, error),
            loop_acquired(User)
    end.

loop_connected(User, OtherLine) ->
    receive                                           
        {request, User, release} ->
            ok = release(OtherLine),
            reply(User, ok),
            loop_released();
        {media, User, Content} ->
            OtherLine ! {media, self(), Content},
            loop_connected(User, OtherLine);
        {media, OtherLine, Content} ->
            User ! {media, self(), Content},
            loop_connected(User, OtherLine)
    end.
            

