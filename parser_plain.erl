-module(parser_plain).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

parse_text(Text) ->
    io:format("Result: ~p~n", [build_command(Text)]).

build_command(Text) ->    
    build_command(Text, []).

build_command(Text, Accum) ->
    Token = binary:split(Text, [<<"\n">>]),
    case length(Token) of
        1 ->
            Accum;
        _ ->
            build_command(Rest, Accum ++ [Token])
    end.
    
    

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything Below this line is purely for testing purposes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_parse_test() ->
    ?assertEqual( true, true ).
