-module(phone).
-export([start/1, off_hook/1, on_hook/1, dial/2, send_media/2]).
-export([init/1]).

% API
start(Line) ->
    spawn(phone, init, [Line]).

off_hook(Pid) -> cast(Pid, off_hook).
on_hook(Pid) -> cast(Pid, on_hook).
dial(Pid, LineNumber) -> cast(Pid, {dial, LineNumber}).
send_media(Pid, Media) -> cast(Pid, Media).



cast(Pid, Message) ->
    Pid ! {cast, Message}.
    
         
init(Line) ->
    line:attach(Line),
    state_on_hook(Line).


state_on_hook(Line) ->
    receive 
        {cast, off_hook} ->
            ok = line:off_hook(Line),
            dial_tone(Line);
        {media, Line, dial} ->
            ringing(Line)
    end.

ring_in_the_room() ->
    % play ringing sound from phone
    ok.


ringing(Line) ->
    ring_in_the_room(),
    line:media(Line, "beeeeep~n"),
    receive 
        {cast, off_hook} ->
            line:media(Line, accept),
            connected(Line, incoming)
    after 1500 ->  
            % hvis vi ikke greier å aquire linja vår så fortsetter
            % ellers så går releaser vi linja og går til on hook
            ringing(Line)                
    end.

dial_tone(Line) ->
    io:format("duuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuh~n", []),
    receive 
        {cast, {dial, LineNumber}} ->
            ok = line:connect(Line, LineNumber),
            line:media(Line, dial),
            connecting(Line)
    after 1500 ->
            dial_tone(Line)
    end.

ring_in_ear() ->
    % make ring sound in headphone
    io:format("duuuuh~n", []).

connecting(Line) ->           
    receive 
        {media, Line, ringing} ->
            ring_in_ear(),
            connecting(Line);
        {media, Line, accept} ->
            connected(Line, outgoing);
        {cast, on_hook} ->
            line:release(Line),
            state_on_hook(Line)
    end.


connected(Line, Direction) ->
    receive 
        {cast, on_hook} when Direction =:= outgoing ->
            line:release(Line),
            state_on_hook(Line);
        {media, Line, Content} ->
            io:format("Got content: ~p~n", [Content]),
            connected(Line, Direction);
        {cast, {send_media, Content}}  ->
            line:media(Line, Content),
            connected(Line, Direction);
        Unhandled ->
            io:format("Unhandled: ~p~n", [Unhandled])
    end.
    
            
           
        
                   
                    
