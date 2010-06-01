-module(key).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(data_bool).
-import(data_binary).
-import(data_list).
-import(data_number).

-import(dict).

wait(Time) ->
      receive
      after
          Time ->
              true
      end.

get_unix_timestamp() ->
    TS = now(),
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

start(Type, Key) ->
    spawn(?MODULE, loop, [Type, Key, nil]).

loop(Type, Key, Value) ->
    receive
        { command, ReturnPid, Action, Data } ->
            case Type of 
                bool ->
                    {Response, NewValue} = data_bool:do_action(Action, Value, Data),
                    ReturnPid ! {ok, Response};
                binary ->
                    {Response, NewValue} = data_binary:do_action(Action, Value, Data),
                    ReturnPid ! {ok, Response};
                list ->
                    {Response, NewValue} = data_list:do_action(Action, Value, Data),
                    ReturnPid ! {ok, Response};
                number ->
                    {Response, NewValue} = data_number:do_action(Action, Value, Data),
                    ReturnPid ! {ok, Response}
            end,
            loop(Type, Key, NewValue);
        { meta, ReturnPid, Action, _ } ->
            case Action of
                type ->
                    ReturnPid ! {ok, Type}
            end,
            loop(Type, Key, Value);
        { operation, ReturnPid, Action, _ } ->
            case Action of
                store ->
                    store_to_disk(Type, Key, Value),
                    ReturnPid ! ok,
                    loop(Type, Key, Value);
                load ->
                    NewValue = load_from_disk(Type, Key),
                    ReturnPid ! ok,
                    loop(Type, Key, NewValue);
                purge ->
                    purge_file(Type, Key),
                    ReturnPid ! ok,
                    loop(Type, Key, Value);
                purge_and_stop ->
                    purge_file(Type, Key),
                    ReturnPid ! ok;
                stop ->
                    ok
            end
    end.

build_filename(Type, Key) ->
    [ "data/", atom_to_list(Type), "/", Key ].

purge_file(Type, Key) ->
    ok = file:delete( build_filename(Type, Key) ).

store_to_disk(Type, Key, Value) ->
    {ok, File} = file:open( build_filename(Type, Key) , write),
    file:write(File, term_to_binary(Value)),
    file:close(File).

load_from_disk(Type, Key) ->
    {ok, Data} = file:read_file(build_filename(Type, Key)),
    binary_to_term(Data).

action(Type, Pid, Action, Value) ->
    Pid ! { Type, self(), Action, Value },
    receive
        Response ->
            Response
    end.

command(Pid, Action) -> 
    action(command, Pid, Action, nil).
command(Pid, Action, Value) -> 
    action(command, Pid, Action, Value).

meta(Pid, Action) -> 
    action(meta, Pid, Action, nil).
meta(Pid, Action, Value) -> 
    action(meta, Pid, Action, Value).
operation(Pid, Action) -> 
    action(operation, Pid, Action, nil).
operation(Pid, Action, Value) -> 
    action(operation, Pid, Action, Value).

% Tests
basic_set_get_test() ->
    Pid = start(bool, "mykey"),
    ?assertEqual( command(Pid, get), {ok, false} ),
    ?assertEqual( command(Pid, set, true), {ok, 1} ),
    ?assertEqual( command(Pid, get), {ok, true} ).

store_load_test() ->
    Pid = start(bool, "mykey"),
    ?assertEqual( command(Pid, set, true), {ok, 1} ),
    ok = operation(Pid, store),
    ok = operation(Pid, load),
    ?assertEqual( command(Pid, get), {ok, true} ).

purge_test() ->
    Type = bool,
    Key = "mykey",
    Filename = build_filename(Type, Key),

    Pid = start(Type, Key),

    ok = operation(Pid, store),
    {Result, _} = file:read_file_info(Filename),
    ?assertEqual( Result, ok ),
    
    wait(20),
    ok = operation(Pid, purge),
    wait(20),
    
    ?assertEqual( file:read_file_info(Filename), {error, enoent} ).
