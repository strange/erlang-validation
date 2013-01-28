-module(validate_messages).

-compile([export_all]).

message(integer, not_integer) ->
    format(<<"Enter a whole number">>);
message(integer, {greater_than, N}) ->
    format(<<"Number must not exceed ~p~n">>, N);
message(Type, {not_equal, _}) ->
    format(<<"They're not equal!">>);
message(_, {Atom, _}) ->
    format(<<"~p~n">>, [Atom]);
message(_, Atom) ->
    format(<<"~p~n">>, [Atom]).

format(S) -> S.
format(S, V) -> unicode:characters_to_binary(io_lib:format(S, [V])).
