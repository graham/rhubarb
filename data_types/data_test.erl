-module(data_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

default() ->
    test.

do_action(Action, nil, Data) ->
    do_action(Action, default(), Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        get ->
            {OldValue, OldValue};
        set ->
            receive
                nothing ->
                    ok
            after 
                50 ->
                    ok
            end,

            {1, Data}
    end.

