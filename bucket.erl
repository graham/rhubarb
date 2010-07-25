-module(bucket).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(key).

start(Name) ->
    key:start(data_bucket, Name).
    
get(Bucket, FindKey) ->
    case key:write(Bucket, find, FindKey) of
        {ok, Key, error} ->
            nil;
        {ok, Key, Pid} ->
            Pid
    end.
    
getc(Bucket, FindKey, DataType) ->
    case key:write(Bucket, find, FindKey) of
        {ok, Key, error} ->
            NewKeyPid = key:start(DataType, FindKey),
            key:write(Bucket, store, {FindKey, NewKeyPid}),
            NewKeyPid;
        {ok, Key, Pid} ->
            Pid
    end.
 
read(Bucket, Key) ->
    Pid = get(Bucket, Key),
    key:read(Pid).
    
q(Bucket, Key, Action) ->
    Pid = get(Bucket, Key),
    key:write(Pid, Action).
    
q(Bucket, Key, Action, Args) ->
    Pid = get(Bucket, Key),
    key:write(Pid, Action, Args).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything Below this line is purely for testing purposes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

