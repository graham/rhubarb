% Whatever!

-import(data_bool).
-import(data_binary).
-import(data_list).
-import(data_number).
-import(make).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
    make:all(),
    Pid = key:start(number, "mykey"),
    key:command(Pid, set, 100),
    key:command(Pid, incr, 100),
    io:format("Result: ~p~n", [key:command(Pid, get)]).
