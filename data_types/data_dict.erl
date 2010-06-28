-module(data_dict).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(dict).

default() ->
    dict:new().

do_action(Action, nil, Data) ->
    do_action(Action, 0, Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        empty ->
            {1, dict:new()};
        find ->
            {dict:find(Data, OldValue), OldValue};
        fetch ->
            {dict:fetch(Data, OldValue), OldValue};
        store ->
            {Key, Value} = Data,
            {1, dict:store(Key, Value, OldValue)};
        erase ->
            {1, dict:erase(Key, OldValue)}
    end.
