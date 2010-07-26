-module(data_bucket).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

default() ->
    ets:new(bucket, [public]).

do_action(Action, nil, Data) ->
    do_action(Action, default(), Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        empty ->
            {1, ets:new(bucket, [])};
        find ->
            case ets:lookup(OldValue, Data) of 
                [{Key, Value}] ->
                    {Value, OldValue};
                [] ->
                    {error, OldValue}
            end;
        fetch ->
            [{Key, Value}] = ets:lookup(OldValue, Data),
            {Value, OldValue};
        store ->
            Result = ets:insert(OldValue, Data),
            {Result, OldValue};
        erase ->
            {ok, OldValue}
    end.
