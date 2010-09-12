-module(key).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(data_bool).
-import(dict).

-include_lib("key.hrl").

start(Type, Key) ->
    DefaultValue = apply(Type, default, []),
    KeyData = #key{type=Type, key=Key, value=DefaultValue},
    KeyPidData = #keyPid{listeners=[]},
    spawn(?MODULE, write_loop, [KeyData, KeyPidData]).

write_loop( KeyData, 
            #keyPid{ write_ref = nil, waiting_client_pid = nil } = KeyPidData) 
            when length(KeyPidData#keyPid.write_queue) > 0 ->
                
    [WriteJob|RestOfQueue] = KeyPidData#keyPid.write_queue,
    KeyPid = self(),
    WritePid = spawn( fun() -> run_write_func_safe(KeyPid, KeyData, KeyPidData, WriteJob) end ),
    NewWriteRef = erlang:monitor(process, WritePid),
    {WaitingClientPid, _, _, _} = WriteJob,
    write_loop(KeyData, KeyPidData#keyPid{ write_queue = RestOfQueue, write_ref = NewWriteRef, waiting_client_pid = WaitingClientPid});
 
write_loop(KeyData, KeyPidData) ->
    receive
        { key_query, Pid, Command, Payload, _Options } ->
            spawn( fun() -> run_query(Pid, self(), KeyData, Command, Payload) end ),
            write_loop(KeyData, KeyPidData);

        { read, Pid, _Options } ->
            Pid ! { rhubarb_response, KeyData#key.key, {ok, KeyData#key.value} },
            write_loop(KeyData, KeyPidData);
            
        { write, Pid, Command, Payload, Options } ->
            NewKeyPidData = KeyPidData#keyPid{ write_queue = KeyPidData#keyPid.write_queue ++ [{Pid, Command, Payload, Options}] },
            write_loop(KeyData, NewKeyPidData);
        
        { flush_write, NewValue, ClientResponse, KeyCommand, _Options } ->
            % check to see if write is still valid (anther write from a different node could have come in).
            NewKeyData = KeyData#key{value = NewValue},

            % check to see if a client is waiting for this write to finish.
            case KeyPidData#keyPid.waiting_client_pid of
                nil ->
                    ok;
                _ ->
                    KeyPidData#keyPid.waiting_client_pid ! { rhubarb_response, KeyData#key.key, {ok, ClientResponse} }
            end,
            erlang:demonitor(KeyPidData#keyPid.write_ref, [flush]),
            % write to disk if you need to.
            
            % let any listeners know that they key has changed and how.

            % if we have any blocking writes (changes) insert the first one into the pending write list.
            case length(KeyPidData#keyPid.blocked_commands) of
                0 ->
                    % cleanup the waiting client if they existed.
                    NewKeyPidData = KeyPidData#keyPid{ listeners = [], write_ref = nil, waiting_client_pid = nil },
                    write_loop(NewKeyData, NewKeyPidData);
                _ ->
                    [HeadBlockedCommand|RestBlockedCommand] = KeyPidData#keyPid.blocked_commands,
                    NewWriteQueue = [HeadBlockedCommand] ++ KeyPidData#keyPid.write_queue,
                    NewKeyPidData = KeyPidData#keyPid{ write_queue = NewWriteQueue, blocked_commands = RestBlockedCommand,
                                                       listeners = [], write_ref = nil, waiting_client_pid = nil },
                    write_loop(NewKeyData, NewKeyPidData)
            end;
            
        { 'DOWN', _WriteRef, process, _Pid, Reason } ->
            case KeyPidData#keyPid.waiting_client_pid of
                nil ->
                    ok;
                _ ->
                    KeyPidData#keyPid.waiting_client_pid ! { rhubarb_response, KeyData#key.key, {error, Reason} }
            end,
            erlang:demonitor(KeyPidData#keyPid.write_ref),
            NewKeyPidData = KeyPidData#keyPid{ write_ref = nil, waiting_client_pid = nil },
            write_loop(KeyData, NewKeyPidData);
            
        { command_blocked, FullCommand } ->
            erlang:demonitor(KeyPidData#keyPid.write_ref, [flush]),
            NewKeyPidData = KeyPidData#keyPid{ blocked_commands = lists:append(KeyPidData#keyPid.blocked_commands, [ FullCommand ]),
                                               write_ref = nil, waiting_client_pid = nil },
            write_loop(KeyData, NewKeyPidData);

        { flush_to_disk, ReturnPid, _Options } ->
            local_disk_io ! { write, ReturnPid, KeyData#key.type, KeyData#key.key, KeyData#key.value, [] },
            write_loop(KeyData, KeyPidData);
        
        { debug, Pid } ->
            Pid ! {debug, KeyData, KeyPidData},
            write_loop(KeyData, KeyPidData)
    end.
                
run_write_func_safe(KeyPid, KeyData, _KeyPidData, FullCommand) ->
    {_Pid, Command, Payload, Options} = FullCommand,
    case apply(KeyData#key.type, do_action, [Command, KeyData#key.value, Payload]) of
        {ClientResponse, NewData} ->
            KeyPid ! { flush_write, NewData, ClientResponse, Command, Options };
        {_ClientResponse, _NewData, command_blocked } ->
            KeyPid ! { command_blocked, FullCommand }
    end.
    
run_query(ClientPid, _KeyPid, KeyData, Command, Payload) ->
    MyPid = self(),
    QueryPid = spawn( fun() ->
                        Result = apply(KeyData#key.type, do_query, [Command, KeyData#key.value, Payload]),
                        MyPid ! { query_result, Result }
                      end),
    WriteRef = erlang:monitor(process, QueryPid),

    receive
        { 'DOWN', WriteRef, process, _Pid, Reason } ->
            erlang:demonitor(WriteRef, [flush]),
            ClientPid ! { rhubarb_query_result, KeyData#key.key, {error, Reason} };
        { query_result, Result } ->
            erlang:demonitor(WriteRef, [flush]),
            ClientPid ! { rhubarb_query_result, KeyData#key.key, {ok, Result} }
    end.


receive_event() ->
    receive 
        { rhubarb_key_event, _Key, Msg } ->
            Msg
    end.

receive_response() ->
    receive
        { rhubarb_response, _Key, {ok, Msg} } ->
            Msg;
        { rhubarb_response, _Key, {error, _Reason} } ->
            error
    end.
    
receive_query() ->
    receive 
        { rhubarb_query_result, _Key, {ok, Result} } ->
            Result;
        { rhubarb_query_result, _Key, {error, _Reason} } ->
            error
    end.

read(Pid) ->
    read(Pid, []).
read(Pid, Options) ->
    Pid ! { read, self(), Options },
    receive_response().

write(Pid, Command) ->
    write(Pid, Command, nil).
write(Pid, Command, Value) ->
    write(Pid, Command, Value, []).
write(Pid, Command, Value, Options) ->
    Pid ! { write, self(), Command, Value, Options },
    receive_response().
    
write_nw(Pid, Command) ->
    write_nw(Pid, Command, nil).
write_nw(Pid, Command, Value) ->
    write_nw(Pid, Command, Value, []).
write_nw(Pid, Command, Value, Options) ->
    Pid ! { write, self(), Command, Value, Options }.
    
blind_write(Pid, Command, Value) ->
    blind_write(Pid, Command, Value, []).
blind_write(Pid, Command, Value, Options) ->
    Pid ! { write, nil, Command, Value, Options}.
    
key_query(Pid, Command) ->
    key_query(Pid, Command, nil).
key_query(Pid, Command, Value) ->
    key_query(Pid, Command, Value, nil).
key_query(Pid, Command, Value, Options) ->
    Pid ! { key_query, self(), Command, Value, Options },
    receive_query().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything Below this line is purely for testing purposes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_read_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { read, self(), [] },
    receive 
        Msg ->
            ?assertEqual( Msg, { rhubarb_response, "MyKey", {ok, false} } )
    end.

basic_write_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { write, self(), set, true, [] },
    receive
        Msg ->
            ?assertEqual( Msg, { rhubarb_response, "MyKey", {ok, 1} } )
    end.

basic_failed_write_test() ->
    Pid = start(data_bool, "MyKey"),
    Result = write(Pid, set, false),
    ?assertEqual( Result, 1 ),
    
    Result2 = read(Pid),
    ?assertEqual( Result2, false ).
    
basic_blind_write_test() ->
    Pid = start(data_test, "key"),
    ?assertEqual( write(Pid, set, "asdf"), 1 ),
    
    blind_write( Pid, set, "woot"),
    ?assertEqual( read(Pid), "asdf" ),
    
    receive
        Msg ->
            io:format("Blind Message: ~p~n", [Msg])
    after
        200 ->
            ?assertEqual( read(Pid), "woot" )
    end.
    
multi_blind_write_test() ->
    Pid = start(data_test, "key"),
    ?assertEqual( write(Pid, set, "asdf"), 1 ),
    
    blind_write( Pid, set, "woot"),
    blind_write( Pid, set, "woot1"),
    blind_write( Pid, set, "woot2"),
    ?assertEqual( read(Pid), "asdf" ),
    
    receive
        _ ->
            ok
    after
        500 ->
            ?assertEqual( read(Pid), "woot2" )
    end.

failed_write_still_read_test() ->
    Pid = start(data_test, "key"),
    
    write(Pid, set, "woot"),
    write(Pid, adsf, "blarg"),
    
    ?assertEqual( read(Pid), "woot" ).
    
blocking_write_test() ->
    Pid = start(data_list, "key"),
    
    spawn( fun() ->
        Value = write(Pid, brpop),
        ?assertEqual( 111, Value)
    end ),
    
    write(Pid, rpush, 111).

run_query_test() ->
    Pid = start(data_list, "key"),
    lists:foreach( fun(Element) -> write(Pid, rpush, Element) end, [1,2,3,4,5] ),
    Result = key_query(Pid, max),
    ?assertEqual( Result, 5 ).

