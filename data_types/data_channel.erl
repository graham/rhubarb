-module(data_channel).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

default() ->
    [].

do_action(Action, nil, Data) ->
    do_action(Action, default(), Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        get ->
            {OldValue, OldValue};
        set ->
            {1, Data};
        add_listener ->
            {1, [Data|OldValue]};
        remove_listener ->
            ok
    end.

