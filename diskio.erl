-module(diskio).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(key).

start() ->
    Pid = spawn(fun() -> loop() end),
    register(local_disk_io, Pid).

loop() ->
    receive
        { write, KeyPid, Type, Key, Value, Options } ->
            WritePid = spawn( fun() -> store_to_disk(Type, Key, Value) end ),
            NewWriteRef = erlang:monitor(process, WritePid),
            wait_for_write(KeyPid, NewWriteRef);
        { read, ReturnPid, Key, Options } ->
            {Type, Value} = load_from_disk(Key),
            ReturnPid ! { load, Type, Value };
        
        { purge, Key, _ReturnPid } ->
            purge_file(Key)
    end.
    
wait_for_write(KeyPid, WriteRef) ->
    receive
        { 'DOWN', WriteRef, process, Pid, normal } ->
            case KeyPid of
                nil ->
                    ok;
                _ ->
                    KeyPid ! { write_to_disk, success }
            end;
        { 'DOWN', WriteRef, process, Pid, Reason } ->
            case KeyPid of
                nil ->
                    ok;
                _ ->
                    KeyPid ! { write_to_disk, error, Reason }
            end
    end,
    
    loop().

    
build_filename(Key) ->
    [ "storage/", Key ].

purge_file(Key) ->
    ok = file:delete( build_filename(Key) ).

store_to_disk(Type, Key, Value) ->
    {ok, File} = file:open( build_filename(Key), write),
    file:write(File, term_to_binary([<<"erlang_term">>, Type, Value])),
    file:close(File).

load_from_disk(Key) ->
    {ok, Data} = file:read_file(build_filename(Key)),
    [StorageFormat, Type, Value] = binary_to_term(Data),
    { Type, Value }.

save_key_to_disk(Pid) ->
    Pid ! { flush_to_disk, self(), [] },
    receive
        {write_to_disk, success} ->
            ok
    end.

load_key_from_disk(Key) ->
    local_disk_io ! { read, self(), Key, [] },
    receive
        { load, Type, Value } ->
            Pid = key:start(Type, Key),
            key:write(Pid, set, Value),
            Pid
    end.

write_to_disk_test() ->
    Pid = key:start(data_test, "MyKey"),
    key:write(Pid, set, "woot2"),
    Return = save_key_to_disk(Pid),
    ?assertEqual( Return, ok ),
    key:write(Pid, set, "asdf"),

    Pid2 = load_key_from_disk("MyKey"),
    ?assertEqual( key:read(Pid2), {ok, "MyKey", "woot2"} ).
    
