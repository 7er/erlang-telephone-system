-module(telephone_tests).
-include_lib("eunit/include/eunit.hrl").


test_off_hook_must_first_attach({OurLineNumber, _}) ->
    fun() ->     
            error = line:off_hook(OurLineNumber),
            ok = line:attach(OurLineNumber),
            ok = line:off_hook(OurLineNumber),
            error = line:off_hook(OurLineNumber)
    end.

test_on_hook({OurLineNumber, _}) ->
    fun() ->
            ok = line:attach(OurLineNumber),
            ok = line:off_hook(OurLineNumber),
            ok = line:on_hook(OurLineNumber),
            error = line:on_hook(OurLineNumber),
            ok = line:off_hook(OurLineNumber)
    end.

dial_should_fail_before_offhook_of_caller({LineNumber, OtherLineNumber}) ->
    fun() ->
            error = line:dial(LineNumber, OtherLineNumber),
            line:attach(LineNumber),
            error = line:dial(LineNumber, OtherLineNumber)            
    end.

dial_only_works_when_both_lines_are_attached_and_caller_is_off_hook({Alice, Bob}) ->
    fun() ->
            ok = line:attach(Alice),
            ok = line:attach(Bob),
            ok = line:off_hook(Alice),
            ok = line:dial(Alice, Bob),
            ok = line:off_hook(Bob),
            

            Alice ! {media, self(), "skviggeribu"},
            BobPid = whereis(Bob),
            receive
                Anything ->
                    ?assertEqual({media, BobPid, "skviggeribu"}, Anything)
            end,
            ok = line:on_hook(Alice)
            
    end.

line_test_() ->
    {foreach, 
     fun() -> setup() end,
     fun({LineNumber, OtherLineNumber}) -> 
             true = exit(whereis(LineNumber), normal),
             true = unregister(LineNumber), 
             true = exit(whereis(OtherLineNumber), normal),
             true = unregister(OtherLineNumber)
     end,
     [
      fun test_off_hook_must_first_attach/1,
      fun test_on_hook/1,
      fun dial_should_fail_before_offhook_of_caller/1,
      fun dial_only_works_when_both_lines_are_attached_and_caller_is_off_hook/1
      % fun should_disconnect_when_we_release_initiator_side_of_connection/1
      %fun simulate_dialing/1
     ]
    }.

setup() ->              
    LineNumber = syver@cisco.com,
    OtherLineNumber = elias@cisco.com,
    line:start(LineNumber),
    line:start(OtherLineNumber),
    {LineNumber, OtherLineNumber}.
    
              
    
