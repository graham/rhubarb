% Whatever!

-import(data_bool).
-import(data_binary).
-import(data_list).
-import(data_number).
-import(make).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
    make:all(),



    ListOfProcs = big_spawn(20000, fun() -> 
                                           Pid = key:start(number, "mykey"),
                                           key:command(Pid, set, 100),
                                           Pid 
                                   end, []),

    io:format("Length: ~p~n~n", [length(ListOfProcs)]),


    {Time, _} = timer:tc(lists, map, [fun(I) -> 
                                              key:command(I, incr, 100)
                                      end, ListOfProcs]),
    io:format("Send: ~p~n", [Time/1000000]),

    {Time2, _} = timer:tc(lists, map, [fun(I) -> 
                                              key:command(I, incr, 100000)
                                      end, ListOfProcs]),
    io:format("Send: ~p~n", [Time2/1000000]),

    {Time3, _} = timer:tc(lists, map, [fun(I) -> 
                                              key:command(I, get),
                                              key:command(I, decr, 100)
                                      end, ListOfProcs]),
    io:format("Send: ~p~n", [Time3/1000000]),



    {TimeKill, _} = timer:tc(lists, map, [fun(I) -> key:operation(I, stop) end, ListOfProcs ]),
    io:format("~nKill: ~p~n", [TimeKill]).

big_spawn(Count, F, Accum) when Count > 0 ->
    NewAcc = [F()|Accum],
    big_spawn(Count-1, F, NewAcc);
big_spawn(0, _, Accum) ->
    Accum.
    

    
