-module(data_scored_set).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

default() ->
    [].

do_action(Action, nil, Data) ->
    do_action(Action, 0, Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        _ ->
            ok
    end.
