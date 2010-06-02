-module(data_binary).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

default() ->
    "".

do_action(Action, nil, Data) ->
    do_action(Action, default(), Data);

do_action(Action, OldValue, Data) when is_list(Data) ->
    do_action(Action, OldValue, list_to_binary(Data));

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        set ->
            {1, Data};
        get ->
            {OldValue, OldValue};
        append ->
            NewValue = <<OldValue/binary, Data/binary>>,
            {size(NewValue), NewValue};
        prepend ->
            NewValue = <<Data/binary, OldValue/binary>>,
            {size(NewValue), NewValue};
        length ->
            {size(OldValue), OldValue}
    end.

binary_kv_test() ->
    ?assertEqual( do_action(get, <<"Graham">>, nil), {<<"Graham">>, <<"Graham">>} ),
    ?assertEqual( do_action(set, <<"Graham">>, <<"Abbott">>), {1, <<"Abbott">>} ).

binary_kv_append_prepend_test() ->
    ?assertEqual( do_action(append, <<"Graham">>, <<"Abbott">>), {12, <<"GrahamAbbott">>} ),
    ?assertEqual( do_action(prepend, <<"Abbott">>, <<"Graham">>), {12, <<"GrahamAbbott">>} ).

binary_kv_length_test() ->
    ?assertEqual( do_action(length, <<"">>, nil), {0, <<"">>} ),
    ?assertEqual( do_action(length, <<"Graham">>, nil), {6, <<"Graham">>} ).


binary_add_list_test() ->
    ?assertEqual( do_action(set, <<"">>, "This is a test"), {1, <<"This is a test">>} ).
