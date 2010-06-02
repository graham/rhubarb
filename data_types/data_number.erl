-module(data_number).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

default() ->
    0.

do_action(Action, nil, Data) ->
    do_action(Action, default(), Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        set ->
            {1, Data};
        get ->
            {OldValue, OldValue};
        incr ->
            NewValue = OldValue + Data,
            {NewValue, NewValue};
        decr ->
            NewValue = OldValue - Data,
            {NewValue, NewValue};
        getset ->
            {OldValue, Data}
    end.


set_get_test() ->
    ?assertEqual( do_action(get, 0, nil), {0, 0} ),
    ?assertEqual( do_action(set, 0, 1), {1, 1} ).

incr_decr_test() ->
    ?assertEqual( do_action(incr, 1, 1), {2, 2} ),
    ?assertEqual( do_action(decr, 2, 1), {1, 1} ),
    ?assertEqual( do_action(incr, 1, 50), {51, 51} ).

getset_test() ->
    ?assertEqual( do_action(getset, 1, 2), {1, 2} ).
