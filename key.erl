-module(key).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(data_bool).
-import(dict).

-include_lib("key.hrl").

start(Type, Key) ->
    DefaultValue = apply(Type, default, []),
    KeyData = #keyData{type=Type, key=Key, value=DefaultValue},
    spawn(?MODULE, loop, [KeyData]).

loop(KeyData) ->
    receive
        { read, Pid } ->
            Pid ! {ok, KeyData#keyData.key, KeyData#keyData.value},
            loop(KeyData);

        { write, Pid, Command, Payload } ->
            write_loop(KeyData, [{Pid, Command, Payload}], nil, nil)
    end.

write_loop(KeyData, [], nil, nil) ->
    receive
        { read, Pid } ->
            Pid ! {ok, KeyData#keyData.key, KeyData#keyData.value},
            write_loop(KeyData, [], nil, nil);
        { write, Pid, Command, Payload } ->
            write_loop(KeyData, [{Pid, Command, Payload}], nil, nil)
    after
        0 ->
            loop(KeyData)
    end;

write_loop(#keyData{safe_writes=true} = KeyData, WriteQueue, nil, nil) ->
    [WriteJob|RestOfQueue] = WriteQueue,
    KeyPid = self(),
    WritePid = spawn( fun() -> run_write_func_safe(KeyPid, KeyData, WriteJob) end ),
    NewWriteRef = erlang:monitor(process, WritePid),
    {WaitingClientPid, _, _} = WriteJob,
    write_loop(KeyData, RestOfQueue, NewWriteRef, WaitingClientPid);

write_loop(#keyData{safe_writes=false} = KeyData, WriteQueue, nil, nil) ->
    [WriteJob|RestOfQueue] = WriteQueue,
    {NewValue, ClientResponse} = run_write_func_unsafe(KeyData, WriteJob),
    {WaitingClientPid, _, _} = WriteJob,
    
    NewKeyData = KeyData#keyData{value = NewValue},
    
    case WaitingClientPid of
        nil ->
            ok;
        _ ->
            WaitingClientPid ! { ok, KeyData#keyData.key, ClientResponse }
    end,
    write_loop(NewKeyData, RestOfQueue, nil, nil);

write_loop(KeyData, WriteQueue, WriteRef, WaitingClientPid) ->
    receive
        { read, Pid } ->
            Pid ! {ok, KeyData#keyData.key, KeyData#keyData.value},
            write_loop(KeyData, WriteQueue, WriteRef, WaitingClientPid);

        { write, Pid, Command, Payload } ->
            write_loop(KeyData, WriteQueue ++ [{Pid, Command, Payload}], WriteRef, WaitingClientPid);
            
        { flush_write, NewValue, ClientResponse } ->
            NewKeyData = KeyData#keyData{value = NewValue},
            case WaitingClientPid of
                nil ->
                    ok;
                _ ->
                    WaitingClientPid ! { ok, NewKeyData#keyData.key, ClientResponse }
            end,
            erlang:demonitor(WriteRef, [flush]),
            write_loop(NewKeyData, WriteQueue, nil, nil);

        { 'DOWN', WriteRef, process, Pid, Reason } ->
            case WaitingClientPid of
                nil ->
                    ok;
                _ ->
                    WaitingClientPid ! { error, KeyData#keyData.key, Reason }
            end,
            erlang:demonitor(WriteRef),
            write_loop(KeyData, WriteQueue, nil, nil)
    end.

run_write_func_unsafe(KeyData, {Pid, Command, Payload}) ->
    {ClientResponse, NewData} = apply(KeyData#keyData.type, do_action, 
                                      [Command, KeyData#keyData.value, Payload]),
    {NewData, ClientResponse}.    
        
run_write_func_safe(KeyPid, KeyData, {Pid, Command, Payload}) ->
    {ClientResponse, NewData} = apply(KeyData#keyData.type, do_action, 
                                      [Command, KeyData#keyData.value, Payload]),
    KeyPid ! { flush_write, NewData, ClientResponse }.
    
% assistant_functions
build_filename(Type, Key) ->
    [ "../storage/", atom_to_list(Type), "/", Key ].

purge_file(Type, Key) ->
    ok = file:delete( build_filename(Type, Key) ).

store_to_disk(Type, Key, Value) ->
    {ok, File} = file:open( build_filename(Type, Key) , write),
    file:write(File, term_to_binary(Value)),
    file:close(File).

load_from_disk(Type, Key) ->
    {ok, Data} = file:read_file(build_filename(Type, Key)),
    binary_to_term(Data).

read(Pid) ->
    Pid ! { read, self() },
    receive
        Msg ->
            Msg
    end.

write(Pid, Command, Value) ->
    Pid ! { write, self(), Command, Value },
    receive
        Msg ->
            Msg
    end.

blind_write(Pid, Command, Value) ->
    Pid ! { write, nil, Command, Value }.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything Below this line is purely for testing purposes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_read_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { read, self() },
    receive 
        Msg ->
            ?assertEqual( Msg, { ok, "MyKey", false } )
    end.

basic_write_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { write, self(), set, true },
    receive
        Msg ->
            ?assertEqual( Msg, { ok, "MyKey", 1 } )
    end.

basic_failed_write_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { write, self(), asdf, "True" },
    receive
        { error, Key, Reason } ->
            ?assertEqual( Key, "MyKey" )
    end,
    
    Pid ! { read, self() },
    receive
        Msg ->
            ?assertEqual( Msg, { ok, "MyKey", false } )
    end.
        
basic_failed_write_still_read_test() ->
    Pid = start(data_test, "MyKey"),

    Pid ! { write, self(), asdf, "True" },
    Pid ! { read, self() },

    receive
        Msg ->
            ?assertEqual( Msg, { ok, "MyKey", test } )
    end,

    receive
        { error, Key, Reason } ->
            ?assertEqual( Key, "MyKey" )
    end.
    
basic_blind_write_test() ->
    Pid = start(data_test, "key"),
    ?assertEqual( write(Pid, set, "asdf"), { ok, "key", 1 } ),
    
    blind_write( Pid, set, "woot"),
    ?assertEqual( read(Pid), { ok, "key", "asdf" } ),
    
    receive
        _ ->
            ok
    after
        200 ->
            ?assertEqual( read(Pid), {ok, "key", "woot"} )
    end.
    
multi_blind_write_test() ->
    Pid = start(data_test, "key"),
    ?assertEqual( write(Pid, set, "asdf"), { ok, "key", 1 } ),
    
    blind_write( Pid, set, "woot"),
    blind_write( Pid, set, "woot1"),
    blind_write( Pid, set, "woot2"),
    ?assertEqual( read(Pid), { ok, "key", "asdf" } ),
    
    receive
        _ ->
            ok
    after
        1000 ->
            ?assertEqual( read(Pid), {ok, "key", "woot2"} )
    end.
