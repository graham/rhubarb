-module(key).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(data_bool).
-import(dict).

-include_lib("key.hrl").

start(Type, Key) ->
    DefaultValue = apply(Type, default, []),
    KeyData = #keyData{type=Type, key=Key, value=DefaultValue},
    spawn(?MODULE, write_loop, [KeyData, [], nil, nil]).

write_loop(#keyData{safe_writes=true} = KeyData, WriteQueue, nil, nil) when length(WriteQueue) > 0 ->
    [WriteJob|RestOfQueue] = WriteQueue,
    KeyPid = self(),
    WritePid = spawn( fun() -> run_write_func_safe(KeyPid, KeyData, WriteJob) end ),
    NewWriteRef = erlang:monitor(process, WritePid),
    {WaitingClientPid, _, _, _} = WriteJob,
    write_loop(KeyData, RestOfQueue, NewWriteRef, WaitingClientPid);

write_loop(#keyData{safe_writes=false} = KeyData, WriteQueue, nil, nil) when length(WriteQueue) > 0 ->
    [WriteJob|RestOfQueue] = WriteQueue,
    {NewValue, ClientResponse, Options} = run_write_func_unsafe(KeyData, WriteJob),
    {WaitingClientPid, _, _, _} = WriteJob,
    
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
        { read, Pid, _Options } ->
            Pid ! {ok, KeyData#keyData.key, KeyData#keyData.value},
            write_loop(KeyData, WriteQueue, WriteRef, WaitingClientPid);

        { write, Pid, Command, Payload, Options } ->
            write_loop(KeyData, WriteQueue ++ [{Pid, Command, Payload, Options}], WriteRef, WaitingClientPid);
            
        { flush_write, NewValue, ClientResponse, _Options } ->
            % here is where you can distribute to other nodes, and do checks to make sure you can write.
            
            
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
            write_loop(KeyData, WriteQueue, nil, nil);
        
        { flush_to_disk, ReturnPid, _Options } ->
            local_disk_io ! { write, ReturnPid, KeyData#keyData.type, KeyData#keyData.key, KeyData#keyData.value, [] },
            write_loop(KeyData, WriteQueue, WriteRef, WaitingClientPid)
    end.

run_write_func_unsafe(KeyData, {Pid, Command, Payload, Options}) ->
    {ClientResponse, NewData} = apply(KeyData#keyData.type, do_action, 
                                      [Command, KeyData#keyData.value, Payload]),
    {NewData, ClientResponse, Options}.    
        
run_write_func_safe(KeyPid, KeyData, {Pid, Command, Payload, Options}) ->
    {ClientResponse, NewData} = apply(KeyData#keyData.type, do_action, 
                                      [Command, KeyData#keyData.value, Payload]),
    KeyPid ! { flush_write, NewData, ClientResponse, Options }.
    

read(Pid) ->
    read(Pid, []).

read(Pid, Options) ->
    Pid ! { read, self(), Options },
    receive
        Msg ->
            Msg
    end.

write(Pid, Command, Value) ->
    write(Pid, Command, Value, []).
    
write(Pid, Command, Value, Options) ->
    Pid ! { write, self(), Command, Value, Options },
    receive
        Msg ->
            Msg
    end.

blind_write(Pid, Command, Value) ->
    blind_write(Pid, Command, Value, []).
    
blind_write(Pid, Command, Value, Options) ->
    Pid ! { write, nil, Command, Value, Options}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything Below this line is purely for testing purposes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_read_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { read, self(), [] },
    receive 
        Msg ->
            ?assertEqual( Msg, { ok, "MyKey", false } )
    end.

basic_write_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { write, self(), set, true, [] },
    receive
        Msg ->
            ?assertEqual( Msg, { ok, "MyKey", 1 } )
    end.

basic_failed_write_test() ->
    Pid = start(data_bool, "MyKey"),
    Result = write(Pid, set, false),
    ?assertEqual( Result, { ok, "MyKey", 1} ),
    
    Result2 = read(Pid),
    ?assertEqual( Result2, {ok, "MyKey", false} ).
    
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

failed_write_still_read_test() ->
    Pid = start(data_test, "key"),
    
    write(Pid, set, "woot"),
    write(Pid, adsf, "blarg"),
    
    ?assertEqual( read(Pid), {ok, "key", "woot"} ).