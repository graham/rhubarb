#! /usr/bin/env escript
%%! -smp disable

% Whatever!

-import(data_bool).
-import(data_binary).
-import(data_list).
-import(data_number).
-import(make).

-include_lib("eunit/include/eunit.hrl").

do( F, Times ) ->
    [F() || _ <- lists:seq(1, Times)].

run_and_time(Command, Func, ListOfProcs) ->
    {Time, _} = timer:tc(lists, map, [Func, ListOfProcs]),
    Mili = Time/1000000,
    Estimate = (1.0 / Mili) * length(ListOfProcs),
    
    io:format("Command: ~s\tTime: ~p \tEstimate Per Second: ~.2f~n", [Command, Mili, Estimate]).

main(_) ->
    make:all(),

    ListOfProcs = big_spawn(5000, fun() -> 
                                           Pid = key:start(data_list, "mykey"),
                                           do( fun() -> key:write(Pid, lpush, 200) end, 100),
                                           Pid
                                   end, []),

    io:format("Length: ~p~n~n", [length(ListOfProcs)]),


    run_and_time("read              ", fun(I) -> key:read(I) end, ListOfProcs),
    run_and_time("write->rpush      ", fun(I) -> key:write(I, rpush, 100) end, ListOfProcs),
    run_and_time("write->rpop       ", fun(I) -> key:write(I, rpop, nil) end, ListOfProcs),
    run_and_time("write->lpush      ", fun(I) -> key:write(I, lpush, 100) end, ListOfProcs),
    run_and_time("write->lpop       ", fun(I) -> key:write(I, lpop, nil) end, ListOfProcs),
    run_and_time("blind_write->rpush", fun(I) -> key:blind_write(I, rpush, 200) end, ListOfProcs),
    run_and_time("read              ", fun(I) -> key:read(I) end, ListOfProcs),
    wait(5000),
    run_and_time("read              ", fun(I) -> key:read(I) end, ListOfProcs),
    ok.

    %{TimeKill, _} = timer:tc(lists, map, [fun(I) -> key:operation(I, stop) end, ListOfProcs ]),
    %io:format("~nKill: ~p~n", [TimeKill]).

big_spawn(Count, F, Accum) when Count > 0 ->
    NewAcc = [F()|Accum],
    big_spawn(Count-1, F, NewAcc);
big_spawn(0, _, Accum) ->
    Accum.

wait(Time) ->
    receive
        _ ->
             ok
    after
        Time ->
            ok
    end.
