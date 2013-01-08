-module(protocol_tests).
-include_lib("eunit/include/eunit.hrl").
 
                                                % maybe change line prototol to be more async

between_line_protocol_test_() ->
    {foreach, 
     fun() -> 
             line:start(bob@example.org)
     end,
     fun(_) -> 
             true = fun(undefined) -> 
                            true;
                       (Pid) ->
                            true = exit(Pid, error),
                            true = unregister(bob@example.org)
                    end(whereis(bob@example.org))
     end,
     [
      fun(_) ->
              {"Alice calls Bob successfully, Bob says Hello and Alice Hangs up", 
              fun() ->
                      register(alice@example.org, self()),
                      line:attach(bob@example.org),
                      BobPid = whereis(bob@example.org),
                      % implement connection to bobs line succeeds
                      ok = gen_fsm:sync_send_event(BobPid, connect),
                      % the following fails because we are not the connecting line
                      line:off_hook(BobPid), % implement bobs side picks up the phone
                      
                      receive 
                          AnswerMessage ->
                              ?assertEqual(answer, decode_event(AnswerMessage))
                      end,
                                                % bob says hello
                      receive
                          Anything ->
                              ?assertEqual({media, BobPid, "Hello"}, Anything)
                      after 100 ->
                              ?debugMsg("No message received"),
                              ?assert(false)
                      end,
                      ok = gen_fsm:sync_send_msg(BobPid, disconnect)
              end}
      end,
      fun(_) ->
              {"Bob calls Alice successfully, Bob says Hello and Hangs up", 
              fun() ->
                      register(alice@example.org, self()),
                      line:attach(bob@example.org),
                      BobPid = whereis(bob@example.org),
                                                % implement bob dialling alice@example.org
                      receive
                          ConnectMessage ->
                              ?assertEqual(connect, decode_sync_event(ConnectMessage)),
                              gen_fsm:reply(decode_sync_event_from(ConnectMessage), ok)
                      after 100 ->
                              ?assert(false)
                      end,
                      gen_fsm:send_event(BobPid, answer),
                                                % implement bob says hello to alice
                      receive
                          Anything ->
                              ?assertEqual({media, BobPid, "Hello"}, Anything)
                      after 100 ->
                              ?assert(false)
                      end,
                                                % implement bob hangs up
                      receive
                          DisconnectMessage ->
                              ?assertEqual(disconnect, decode_sync_event(DisconnectMessage)),
                              gen_fsm:reply(decode_sync_event_from(DisconnectMessage), ok)
                      after 100 ->
                              ?assert(false)
                      end
              end}
      end
     ]
    }.


decode_sync_event({'$gen_sync_event', _, Event}) -> Event.
decode_event({'$gen_event', Event}) -> Event.
decode_sync_event_from({'$gen_sync_event', From, _}) -> From.

