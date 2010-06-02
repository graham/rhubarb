-module(data_bitmap).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

default() ->
    "".

do_action(Action, nil, Data) ->
    do_action(Action, default(), Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    ok.
