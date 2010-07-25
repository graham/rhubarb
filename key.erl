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

write_loop( #key{safe_writes=true} = KeyData, 
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
        { read, Pid, _Options } ->
            Pid ! { self(), {ok, KeyData#key.key, KeyData#key.value} },
            write_loop(KeyData, KeyPidData);

        { write, Pid, Command, Payload, Options } ->
            NewKeyPidData = KeyPidData#keyPid{ write_queue = KeyPidData#keyPid.write_queue ++ [{Pid, Command, Payload, Options}] },
            write_loop(KeyData, NewKeyPidData);
        
        { flush_write, NewValue, ClientResponse, _Options } ->
            % check to see if write is still valid (anther write from a different node could have come in).
            NewKeyData = KeyData#key{value = NewValue},

            % check to see if a client is waiting for this write to finish.
            case KeyPidData#keyPid.waiting_client_pid of
                nil ->
                    ok;
                _ ->
                    KeyPidData#keyPid.waiting_client_pid ! { self(), {ok, KeyData#key.key, ClientResponse} }
            end,
            erlang:demonitor(KeyPidData#keyPid.write_ref, [flush]),
            % write to disk if you need to.
            
            % let any listeners know that they key has changed and how.
            NewListeners = send_update_event(KeyPidData#keyPid.listeners, {KeyData#key.key, ClientResponse}),

            % if we have any blocking writes (changes) insert the first one into the pending write list.
            case length(KeyPidData#keyPid.blocked_commands) of
                0 ->
                    % cleanup the waiting client if they existed.
                    NewKeyPidData = KeyPidData#keyPid{ listeners = NewListeners, write_ref = nil, waiting_client_pid = nil },
                    write_loop(NewKeyData, NewKeyPidData);
                _ ->
                    [HeadBlockedCommand|RestBlockedCommand] = KeyPidData#keyPid.blocked_commands,
                    NewWriteQueue = [HeadBlockedCommand] ++ KeyPidData#keyPid.write_queue,
                    NewKeyPidData = KeyPidData#keyPid{ write_queue = NewWriteQueue, blocked_commands = RestBlockedCommand,
                                                       listeners = NewListeners, write_ref = nil, waiting_client_pid = nil },
                    write_loop(NewKeyData, NewKeyPidData)
            end;
            
        { 'DOWN', WriteRef, process, Pid, Reason } ->
            case KeyPidData#keyPid.waiting_client_pid of
                nil ->
                    ok;
                _ ->
                    KeyPidData#keyPid.waiting_client_pid ! { self(), {error, KeyData#key.key, Reason} }
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
        
        { add_listener, Listener, _Options } ->
            NewKeyPidData = KeyPidData#keyPid{ listeners = KeyPidData#keyPid.listeners ++ [Listener] },
            write_loop(KeyData, NewKeyPidData);
            
        { remove_listener, Listener, _Options } ->
            NewKeyPidData = KeyPidData#keyPid{ listeners = lists:delete(Listener, KeyPidData#keyPid.listeners) },
            write_loop(KeyData, NewKeyPidData);
            
        { debug, Pid } ->
            Pid ! {debug, KeyData, KeyPidData},
            write_loop(KeyData, KeyPidData)
    end.

                
run_write_func_safe(KeyPid, KeyData, KeyPidData, FullCommand) ->
    {Pid, Command, Payload, Options} = FullCommand,
    case apply(KeyData#key.type, do_action, [Command, KeyData#key.value, Payload]) of
        {ClientResponse, NewData} ->
            KeyPid ! { flush_write, NewData, ClientResponse, Options };
        {ClientResponse, NewData, command_blocked } ->
            KeyPid ! { command_blocked, FullCommand }
    end.

send_update_event(Listeners, Message) ->
    io:format("Sending to listeners: ~p:~p~n", [Listeners, Message]),
    lists:foreach( fun(Element) ->
        Element ! { rhubarb_key_event, Message }
    end, Listeners),
    
    Listeners.

read(Pid) ->
    read(Pid, []).

read(Pid, Options) ->
    Pid ! { read, self(), Options },
    receive
        {Pid, Msg} ->
            Msg
    end.

write(Pid, Command) ->
    write(Pid, Command, nil).

write(Pid, Command, Value) ->
    write(Pid, Command, Value, []).
    
write(Pid, Command, Value, Options) ->
    Pid ! { write, self(), Command, Value, Options },
    receive
        {Pid, Msg} ->
            Msg
    end.

blind_write(Pid, Command, Value) ->
    blind_write(Pid, Command, Value, []).
    
blind_write(Pid, Command, Value, Options) ->
    Pid ! { write, nil, Command, Value, Options}.

add_listener(Pid, Listener) ->
    Pid ! { add_listener, Listener, nil}.

remove_listener(Pid, Listener) ->
    Pid ! { remove_listener, Listener, nil}.
    
receive_event() ->
    receive 
        { rhubarb_key_event, Msg } ->
            Msg
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything Below this line is purely for testing purposes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_read_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { read, self(), [] },
    receive 
        Msg ->
            ?assertEqual( Msg, { Pid, {ok, "MyKey", false} } )
    end.

basic_write_test() ->
    Pid = start(data_bool, "MyKey"),
    Pid ! { write, self(), set, true, [] },
    receive
        Msg ->
            ?assertEqual( Msg, { Pid, {ok, "MyKey", 1} } )
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
        Msg ->
            io:format("Blind Message: ~p~n", [Msg])
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
        500 ->
            ?assertEqual( read(Pid), {ok, "key", "woot2"} )
    end.

failed_write_still_read_test() ->
    Pid = start(data_test, "key"),
    
    write(Pid, set, "woot"),
    write(Pid, adsf, "blarg"),
    
    ?assertEqual( read(Pid), {ok, "key", "woot"} ).
    
events_on_change_test() ->
    Pid = start(data_list, "key"),
    
    write(Pid, rpush, 111),
    write(Pid, rpush, 222),
    
    add_listener(Pid, self()),
    
    write(Pid, rpush, 333),
    
    Event = receive_event(),
    
    ?assertEqual( {"key", 3}, Event ).
    
blocking_write_test() ->
    Pid = start(data_list, "key"),
    
    spawn( fun() ->
        Value = write(Pid, brpop),
        ?assertEqual( {ok, "key", 111}, Value)
    end ),
    
    write(Pid, rpush, 111).

