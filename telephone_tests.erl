-module(telephone_tests).
-include_lib("eunit/include/eunit.hrl").

test_line_acquire({{OurLine, _}, _}) ->
    fun() ->            
            ok = line:acquire(OurLine),
            error = line:acquire(OurLine)
    end.

test_line_release({{OurLine, _}, _}) ->
    fun() ->
            ok = line:acquire(OurLine),
            ok = line:release(OurLine),
            error = line:release(OurLine),
            ok = line:acquire(OurLine)
    end.

test_line_connect({{OurLine, _}, {OtherLine ,_}}) ->
    fun() ->
            error = line:connect(OurLine, OtherLine)
    end.
        


single_line_test_() ->
    {foreach, 
     fun() -> setup() end,
     fun({{Pid, LineNumber}, {Other_Pid, Other_LineNumber}}) -> 
             true = unregister(LineNumber), true = exit(Pid, normal), 
             true = unregister(Other_LineNumber), true = exit(Other_Pid, normal) end,
     [
      fun test_line_acquire/1,
      fun test_line_release/1,
      fun test_line_connect/1
     ]
    }.

setup() ->              
    LineNumber = syver@cisco.com,
    Other_LineNumber = elias@cisco.com,
    {{line:start(LineNumber), LineNumber}, {line:start(Other_LineNumber), Other_LineNumber}}.
    
              
    
