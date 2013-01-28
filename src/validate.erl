-module(validate).

-compile([export_all]).

%% http://stackoverflow.com/questions/11718898/check-string-for-email-with-regular-expressions-or-other-way
-define(EMAIL_RE, "\\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*\\b").

-define(DATE_FMT, <<"Y-m-d">>).
-define(TIME_FMT, <<"H:M:S">>).
-define(DATETIME_FMT, <<"Y-m-d H:M:S">>).

-record(args, {
        default = <<>>,
        required = true,
        strict = false,
        trim = true,
        false = [<<>>, [], <<"0">>, "0", 0, <<"off">>, "off", <<"no">>, "no",
            false, nil, null, none, undefined],
        validations = []
    }).

-define(is_empty(V), V =:= [] orelse V =:= <<>> orelse V =:= undefined).
-define(is_comparison(V),
    V =:= lt orelse V =:= lte orelse V =:= gt orelse V =:= gte).

value(Value, Type) -> value(Value, Type, []).
value(Value, Type, Args) ->
    A = args(Args),
    V = maybe_trim(Value, Type, A),
    validations(convert(check_empty(V, Type, A), Type, A), Type, A).

proplist(Rules, Data) -> proplist(Rules, Data, [], []).
proplist([], _Data, Clean, []) -> {ok, Clean};
proplist([], _Data, _Clean, Errors) -> {errors, Errors};
proplist([{Key, Type, Opts}|T], Data, Valid, Errors) ->
    Value = proplists:get_value(Key, Data, <<>>),
    case value(Value, Type, Opts) of
        {Status, Converted} when Status =:= valid; Status =:= default ->
            proplist(T, Data, [{Key, Converted}|Valid], Errors);
        {invalid, Reason} ->
            proplist(T, Data, Valid, [{Key, Reason}|Errors])
    end.

%% @doc Return trimmed value if args dictate it so.
maybe_trim(Value, _Type, #args{trim = true}) -> trim(Value);
maybe_trim(Value, _Type, #args{trim = false}) -> Value.

%% @doc Check if value is empty
check_empty(Value, boolean, _Args) ->
    {valid, Value};
check_empty(Value, _Type, #args{required = true}) when ?is_empty(Value) ->
    {invalid, empty};
check_empty(Value, _Type, #args{default = Default}) when ?is_empty(Value) ->
    {default, Default};
check_empty(Value, _Type, _Args) ->
    {valid, Value}.

%% @doc Coerce value to suitable type.
convert({default, Value}, _Type, _Args) ->
    {default, Value};
convert({invalid, Value}, _Type, _Args) ->
    {invalid, Value};
convert({valid, Value}, integer, _Args) when is_integer(Value) ->
    {valid, Value};
convert({valid, Value}, integer, #args{strict = false} = Args)
        when is_binary(Value) ->
    convert({valid, binary_to_list(Value)}, integer, Args);
convert({valid, Value}, integer, #args{strict = false}) ->
    case catch list_to_integer(Value) of
        {'EXIT', {badarg, _}} -> {invalid, not_integer};
        Converted -> {valid, Converted}
    end;
convert({valid, _Value}, integer, _Args) ->
    {invalid, not_integer};
convert({valid, Value}, string, Args) when is_list(Value) ->
    convert({valid, unicode:characters_to_binary(Value)}, string, Args);
convert({valid, Value}, string, _Args) when is_binary(Value) ->
    case unicode:characters_to_list(Value, utf8) of
        {error, _Converted, _Failed} -> {invalid, not_string};
        _ -> {valid, Value}
    end;
convert(_Value, string, _Args) ->
    {invalid, not_string};
convert({valid, Value}, decimal, #args{strict = true})
        when not is_binary(Value) ->
    {invalid, not_decimal};
convert({valid, Value}, decimal, _Args) when is_tuple(Value) ->
    {invalid, not_decimal};
convert({valid, Value}, decimal, _Args) ->
    case decimal_conv:number(Value) of
        {0, qNaN} -> {invalid, not_decimal};
        Decimal -> {valid, list_to_binary(decimal:format(Decimal))}
    end;
convert({valid, Value}, Fun, Args) when is_function(Fun) ->
    Fun(Value, Args);
convert({valid, Value}, boolean, #args{strict = false, false = FV}) ->
    {valid, not lists:member(Value, FV)};
convert({valid, Value}, boolean, _Args)
        when Value =:= true; Value =:= false ->
    {valid, Value};
convert({valid, _Value}, boolean, _Args) ->
    {invalid, not_boolean};
convert({valid, Value}, date, Args) ->
        convert({valid, Value}, {date, ?DATE_FMT}, Args);
convert({valid, Value}, {date, _Fmt}, _Args) when is_tuple(Value) ->
    case catch datestring_validate:valid_date(Value) of
        {ok, Date} -> {valid, Date};
        _ -> {invalid, not_date}
    end;
convert({valid, Value}, {date, Fmt}, #args{strict = false}) ->
    case catch datestring:parse_date(Fmt, Value) of
        {ok, Date} -> {valid, Date};
        {error, {no_match, _}} -> {invalid, no_match};
        {error, no_match} -> {invalid, no_match};
        _ -> {invalid, not_date}
    end;
convert({valid, _}, {date, _Fmt}, _Args) ->
    {invalid, not_date};
convert({valid, Value}, time, Args) ->
        convert({valid, Value}, {time, ?TIME_FMT}, Args);
convert({valid, Value}, {time, _Fmt}, _Args) when is_tuple(Value) ->
    case catch datestring_validate:valid_time(Value) of
        {ok, Time} -> {valid, Time};
        _ -> {invalid, not_time}
    end;
convert({valid, Value}, {time, Fmt}, #args{strict = false}) ->
    case catch datestring:parse_time(Fmt, Value) of
        {ok, Time} -> {valid, Time};
        {error, {no_match, _}} -> {invalid, no_match};
        {error, no_match} -> {invalid, no_match};
        _ -> {invalid, not_time}
    end;
convert({valid, _}, {time, _Fmt}, _Args) ->
    {invalid, not_time};
convert({valid, Value}, {datetime, _Fmt}, _Args) when is_tuple(Value) ->
    case catch datestring_validate:valid_datetime(Value) of
        {ok, DateTime} -> {valid, DateTime};
        _ -> {invalid, not_datetime}
    end;
convert({valid, Value}, datetime, Args) ->
        convert({valid, Value}, {datetime, ?DATETIME_FMT}, Args);
convert({valid, Value}, {datetime, Fmt}, #args{strict = false}) ->
    case catch datestring:parse_datetime(Fmt, Value) of
        {ok, DateTime} -> {valid, DateTime};
        {error, {no_match, _}} -> {invalid, no_match};
        {error, no_match} -> {invalid, no_match};
        _ -> {invalid, not_datetime}
    end;
convert({valid, _}, {datetime, _Fmt}, _Args) ->
    {invalid, not_datetime};
convert({valid, Value}, json, Args) when is_list(Value) ->
    convert({valid, unicode:characters_to_binary(Value)}, json, Args);
convert({valid, Value}, json, _Args) when is_binary(Value) ->
    case catch jiffy:decode(Value) of
        {error, _Reason} -> {invalid, not_json};
        Object -> {valid, Object}
    end;
convert(_Value, _Type, _Args) ->
    {invalid, unknown_data_type}.

%% @doc Perform validations
validations(Value, Type, #args{validations = Validations}) ->
    validations(Value, Type, Validations);
validations({default, Value}, _Type, _Args) ->
    {default, Value};
validations({invalid, Value}, _Type, _Args) ->
    {invalid, Value};
validations(Value, _Type, []) ->
    Value;
validations({valid, Value}, integer, [{min, N}|_]) when Value < N ->
    {invalid, {less_than, N}};
validations({valid, Value}, integer, [{max, N}|_]) when Value > N ->
    {invalid, {greater_than, N}};
validations({valid, Value}, integer, [{range, Min, Max}|T]) ->
    validations({valid, Value}, integer, [{min, Min}, {max, Max}|T]);
validations({valid, Value}, string, [{min_len, N}|T]) ->
    case length(unicode:characters_to_list(Value, utf8)) of
        Length when Length < N -> {invalid, {shorter_than, N}};
        _ -> validations({valid, Value}, string, T)
    end;
validations({valid, Value}, string, [{max_len, N}|T]) ->
    case length(unicode:characters_to_list(Value, utf8)) of
        Length when Length > N -> {invalid, {longer_than, N}};
        _ -> validations({valid, Value}, string, T)
    end;
validations({valid, Value}, string, [{match, Re}|T]) ->
    case re:run(Value, Re) of
        {match, _} -> validations({valid, Value}, string, T);
        _ -> {invalid, no_match}
    end;
validations({valid, Value}, string, [email|T]) ->
    % ?EMAIL_RE was just copy/pasted from SO. i haven't thoroughly evaluated
    % how it performs. the test should probably be replaced with a custom
    % email validation module.
    case re:run(Value, ?EMAIL_RE) of
        {match, _} -> validations({valid, Value}, string, T);
        _ -> {invalid, not_email}
    end;
validations({valid, Value}, string, [url|T]) ->
    % http_uri:parse/1 will, among other things, not validate TLD. URL
    % validation might get stricter in the fututre!
    case http_uri:parse(unicode:characters_to_list(Value)) of
        {ok, _} -> validations({valid, Value}, string, T);
        _ -> {invalid, not_url}
    end;
validations({valid, Value}, decimal, [{min, N}|T]) ->
    case decimal:compare(Value, N) of
        -1 -> {invalid, {less_than, N}};
        _ -> validations({valid, Value}, decimal, T)
    end;
validations({valid, Value}, decimal, [{max, N}|T]) ->
    case decimal:compare(Value, N) of
        1 -> {invalid, {greater_than, N}};
        _ -> validations({valid, Value}, decimal, T)
    end;
validations({valid, Value}, decimal, [{range, Min, Max}|T]) ->
    validations({valid, Value}, decimal, [{min, Min}, {max, Max}|T]);
validations({valid, Value}, {date, _} = Type, [{Comparison, D}|T])
        when ?is_comparison(Comparison) ->
    case compare(Comparison, Value, D, fun date_to_us/1) of
        {valid, _} -> validations({valid, Value}, Type, T);
        Invalid -> Invalid
    end;
validations({valid, Value}, {date, _} = Type, [{range, GTE, LTE}|T]) ->
    validations({valid, Value}, Type, [{gte, GTE}, {lte, LTE}|T]);
validations({valid, Value}, {time, _} = Type, [{Comparison, D}|T])
        when ?is_comparison(Comparison) ->
    case compare(Comparison, Value, D, fun time_to_us/1) of
        {valid, _} -> validations({valid, Value}, Type, T);
        Invalid -> Invalid
    end;
validations({valid, Value}, {time, _} = Type, [{range, GTE, LTE}|T]) ->
    validations({valid, Value}, Type, [{gte, GTE}, {lte, LTE}|T]);
validations({valid, Value}, {datetime, _} = Type, [{Comparison, D}|T])
        when ?is_comparison(Comparison) ->
    case compare(Comparison, Value, D, fun datetime_to_us/1) of
        {valid, _} -> validations({valid, Value}, Type, T);
        Invalid -> Invalid
    end;
validations({valid, Value}, {datetime, _} = Type, [{range, GTE, LTE}|T]) ->
    validations({valid, Value}, Type, [{gte, GTE}, {lte, LTE}|T]);
validations({valid, Value}, Type, [{in, Haystack}|T]) ->
    case lists:member(Value, Haystack) of
        false -> {invalid, not_in_list};
        true -> validations({valid, Value}, Type, T)
    end;
validations({valid, Value}, Type, [{key_in, Haystack}|T]) ->
    case lists:keyfind(Value, 1, Haystack) of
        false -> {invalid, not_in_list};
        _Tuple -> validations({valid, Value}, Type, T)
    end;
validations({valid, Value}, Type, [Fun|T]) when is_function(Fun) ->
    case Fun(Value, []) of
        {invalid, Reason} -> {invalid, Reason};
        {valid, NewValue} -> validations({valid, NewValue}, Type, T)
    end;
validations({valid, Value}, Type, [{Fun, Args}|T]) when is_function(Fun) ->
    case Fun(Value, Args) of
        {invalid, Reason} -> {invalid, Reason};
        {valid, NewValue} -> validations({valid, NewValue}, Type, T)
    end;
validations({valid, Value}, _Type, [{eq, Eq}|_]) when Value =/= Eq ->
    {invalid, {not_equal, Eq}};
validations(Value, Type, [_|T]) ->
    validations(Value, Type, T).

%% Helpers

compare(Comparison, A, B, Fun) when is_function(Fun) ->
    case compare(Comparison, Fun(A), Fun(B)) of
        {invalid, {FailedComparison, _}} -> {invalid, {FailedComparison, B}};
        {valid, _} -> {valid, A}
    end.
compare(lt, A, B) when A >= B -> {invalid, {greater_than, B}};
compare(lte, A, B) when A > B -> {invalid, {greater_than, B}};
compare(gt, A, B) when A =< B -> {invalid, {less_than, B}};
compare(gte, A, B) when A < B -> {invalid, {less_than, B}};
compare(_Comparison, A, _B) -> {valid, A}.

time_to_us({H, M, S}) -> time_to_us({{H, M, S}, 0});
time_to_us({{0, M, S}, U}) -> time_to_us({{24, M, S}, U});
time_to_us({Time, U}) ->
    calendar:time_to_seconds(Time) * 1000000 + U.

date_to_us({Y, M, D}) ->
    calendar:datetime_to_gregorian_seconds({{Y, M, D}, {0, 0, 0}}) * 1000000.

datetime_to_us({Date, Time}) ->
    date_to_us(Date) + time_to_us(Time).

%% @doc Remove white space characters from binary string.
trim(S) when is_binary(S) ->
    Trimmed = trim(unicode:characters_to_list(S, utf8), left),
    unicode:characters_to_binary(Trimmed, utf8);
trim(S) when is_list(S) ->
    trim(S, left);
trim(S) -> S.

trim([], _Direction) -> [];
trim([$\s|T], Direction) -> trim(T, Direction);
trim([$\n|T], Direction) -> trim(T, Direction);
trim([$\r|T], Direction) -> trim(T, Direction);
trim([$\t|T], Direction) -> trim(T, Direction);
trim(S, left) -> trim(lists:reverse(S), right);
trim(S, right) -> lists:reverse(S).

%% @doc Clean up arguments and return record.
args(Args) -> args(Args, #args{}).
args([], #args{validations = Validators} = Args) ->
    Args#args{validations = lists:reverse(Validators)};
args([required|T], Args) ->
    args(T, Args#args{required = true});
args([{required, Value}|T], Args) ->
    args(T, Args#args{required = Value});
args([trim|T], Args) ->
    args(T, Args#args{trim = true});
args([{trim, Value}|T], Args) ->
    args(T, Args#args{trim = Value});
args([strict|T], Args) ->
    args(T, Args#args{strict = true});
args([{strict, Value}|T], Args) ->
    args(T, Args#args{strict = Value});
args([{default, Value}|T], Args) ->
    args(T, Args#args{default = Value});
args([{false, Values}|T], Args) ->
    args(T, Args#args{false = Values});
args([H|T], #args{validations = Validators} = Args) ->
    args(T, Args#args{validations = [H|Validators]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integer_test() ->
    ?assertEqual({invalid, empty},
        value(<<>>, integer)),
    ?assertEqual({default, <<>>},
        value(<<>>, integer, [{required, false}])),
    ?assertEqual({valid, 1},
        value(<<"1">>, integer, [{required, false}])),

    ?assertEqual({valid, 1},
        value(<<"1">>, integer)),
    ?assertEqual({invalid, not_integer},
        value(<<"1">>, integer, [strict])),

    ?assertEqual({invalid, {greater_than, 0}},
        value(<<"1">>, integer, [{max, 0}])),
    ?assertEqual({invalid, {less_than, 0}},
        value(<<"-1">>, integer, [{min, 0}])),

    ?assertEqual({invalid, {greater_than, 2}},
        value(<<"3">>, integer, [{range, 1, 2}])),
    ?assertEqual({invalid, {less_than, 1}},
        value(<<"0">>, integer, [{range, 1, 2}])),
    ?assertEqual({valid, 1},
        value(<<"1">>, integer, [{range, 1, 2}])),
    ok.

string_test() ->
    ?assertEqual({invalid, empty},
        value(<<>>, string)),
    ?assertEqual({invalid, empty},
        value([], string)),
    ?assertEqual({default, <<>>},
        value([], string, [{required, false}])),

    ?assertEqual({valid, <<"hello">>},
        value(<<"hello">>, string)),

    ?assertEqual({valid, <<" hello ">>},
        value(<<" hello ">>, string, [{trim, false}])),
    ?assertEqual({valid, <<"hello">>},
        value(<<" hello ">>, string, [{trim, true}])),
    
    ?assertEqual({invalid, {shorter_than, 6}},
        value(<<"hello">>, string, [{min_len, 6}])),
    ?assertEqual({invalid, {longer_than, 2}},
        value(<<"hello">>, string, [{max_len, 2}])),
    ?assertEqual({valid, <<"hello">>},
        value(<<"hello">>, string, [{max_len, 5}])),
    ?assertEqual({valid, <<"hello">>},
        value(<<"hello">>, string, [{min_len, 5}])),

    ?assertEqual({valid, <<"hello">>},
        value(<<"hello">>, string, [{eq, <<"hello">>}])),
    ?assertEqual({invalid, {not_equal, <<"world">>}},
        value(<<"hello">>, string, [{eq, <<"world">>}])),
    ok.

string_email_test() ->
    ?assertEqual({valid, <<"gs@pipsq.com">>},
        value(<<"gs@pipsq.com">>, string, [email])),
    ?assertEqual({valid, <<"gs+test@pipsq.com">>},
        value(<<"gs+test@pipsq.com">>, string, [email])),
    ?assertEqual({invalid, not_email},
        value(<<"gs.pipsq.com">>, string, [email])),
    ok.

string_regex_test() ->
    ?assertEqual({valid, <<"abc">>},
        value(<<"abc">>, string, [{match, "^[a-z]+$"}])),
    ?assertEqual({invalid, no_match},
        value(<<"abc123">>, string, [{match, "^[a-z]+$"}])),
    ok.

string_url_test() ->
    ?assertEqual({valid, <<"http://www.google.com/">>},
        value(<<"http://www.google.com/">>, string, [url])),
    ?assertEqual({valid, <<"https://www.google.com/">>},
        value(<<"https://www.google.com/">>, string, [url])),
    ?assertEqual({valid, <<"ftp://www.google.com/">>},
        value(<<"ftp://www.google.com/">>, string, [url])),
    ?assertEqual({invalid, not_url},
        value(<<"lol://www.google.com/">>, string, [url])),
    ?assertEqual({invalid, not_url},
        value(<<"www.google.com/">>, string, [url])),
    ok.

decimal_test() ->
    ?assertEqual({invalid, empty},
        value(<<>>, decimal)),
    ?assertEqual({invalid, not_decimal},
        value(<<"b">>, decimal)),
    ?assertEqual({invalid, not_decimal},
        value(1.0, decimal, [strict])),

    ?assertEqual({invalid, {less_than, <<"2.0">>}},
        value(<<"1.0">>, decimal, [{min, <<"2.0">>}])),
    ?assertEqual({invalid, {greater_than, <<"3.0">>}},
        value(<<"4.5">>, decimal, [{max, <<"3.0">>}])),
    ?assertEqual({invalid, {greater_than, <<"-1.0">>}},
        value(<<"1.0">>, decimal, [{max, <<"-1.0">>}])),

    ?assertEqual({valid, <<"1.0">>},
        value(<<"1.0">>, decimal)),
    ?assertEqual({valid, <<"3.1415926535897932384626433832795028841971">>},
        value(<<"3.1415926535897932384626433832795028841971">>, decimal)),

    ?assertEqual({invalid, not_decimal},
        value(test, decimal, [strict])),
    ?assertEqual({invalid, not_decimal},
        value({test, test}, decimal)),
    ?assertEqual({invalid, not_decimal},
        value({test, test, test}, decimal)),
    ?assertEqual({invalid, not_decimal},
        value({1, 2}, decimal)),

    ok.

json_test() ->
    ?assertEqual({valid, {[{<<"foo">>,<<"bar">>}]}},
        value(<<"{\"foo\": \"bar\"}">>, json)),
    ?assertEqual({valid, {[{<<"foo">>, 1}]}},
        value(<<"{\"foo\": 1}">>, json)),
    ?assertEqual({invalid, not_json},
        value(<<"hello!">>, json)),
    ?assertEqual({invalid, not_json},
        value(<<"{\"foo: 1}">>, json)),
    ?assertEqual({valid, [1, {[{<<"foo">>,<<"2">>}]}]},
        value(<<"[1, {\"foo\": \"2\"}]">>, json)),
    ok.

date_test() ->
    ?assertEqual({invalid, no_match},
        value(<<"1">>, {date, <<"Y">>})),
    ?assertEqual({invalid, not_date},
        value(<<"2012-13-24">>, {date, <<"Y-m-d">>})),

    ?assertEqual({valid, {2012, 12, 24}},
        value(<<"2012-12-24">>, date)),
    ?assertEqual({valid, {2012, 12, 24}},
        value(<<"24th Dec 2012">>, {date, <<"do b Y">>})),
    ?assertEqual({valid, {2012, 12, 24}},
        value(<<"2012-12-24">>, {date, <<"Y-m-d">>})),

    ?assertEqual({invalid, not_date},
        value(<<"24th Dec 2012">>, {date, <<"do b Y">>}, [strict])),
    ?assertEqual({valid, {2012, 12, 24}},
        value({2012, 12, 24}, date, [strict])),
    ?assertEqual({invalid, not_date},
        value({2012, 13, 24}, date, [strict])),
    ?assertEqual({invalid, not_date},
        value(abc, date, [strict])),
    ?assertEqual({invalid, no_match},
        value(abc, date)),

    ?assertEqual({valid, {2012, 12, 24}},
        value(<<"2012-12-24">>, {date, <<"Y-m-d">>}, [{lt, {2012, 12, 30}}])),
    ?assertEqual({invalid, {greater_than, {2012, 12, 30}}},
        value(<<"2012-12-31">>, {date, <<"Y-m-d">>}, [{lt, {2012, 12, 30}}])),
    ?assertEqual({valid, {2012, 12, 30}},
        value(<<"2012-12-30">>, {date, <<"Y-m-d">>}, [{lte, {2012, 12, 30}}])),

    ?assertEqual({valid, {2012, 12, 24}},
        value(<<"2012-12-24">>, {date, <<"Y-m-d">>}, [{gt, {2012, 12, 23}}])),
    ?assertEqual({invalid, {less_than, {2012, 12, 30}}},
        value(<<"2012-12-24">>, {date, <<"Y-m-d">>}, [{gt, {2012, 12, 30}}])),
    ?assertEqual({valid, {2012, 12, 30}},
        value(<<"2012-12-30">>, {date, <<"Y-m-d">>}, [{gte, {2012, 12, 30}}])),
    ?assertEqual({valid, {2012, 12, 24}},
        value(<<"2012-12-24">>, date, [{range, {2012, 12, 23}, {2012, 12, 31}}])),
    ok.

time_test() ->
    ?assertEqual({invalid, no_match},
        value(<<"1">>, {date, <<"H:M">>})),
    ?assertEqual({invalid, not_time},
        value(<<"25:30">>, {time, <<"H:M">>})),

    ?assertEqual({valid, {{13, 20, 20}, 0}},
        value(<<"1:20:20 p.m.">>, {time, <<"l:M:S P">>})),
    ?assertEqual({valid, {{0, 0, 0}, 0}},
        value(<<"00:00:00">>, {time, <<"H:M:S">>})),

    ?assertEqual({valid, {{0, 0, 0}, 0}},
        value({{0, 0, 0}, 0}, {time, nil}, [strict])),
    ?assertEqual({valid, {{0, 0, 0}, 0}},
        value({0, 0, 0}, {time, nil}, [strict])),
    ?assertEqual({valid, {{0, 0, 0}, 0}},
        value({0, 0, 0}, {time, nil})),
    ?assertEqual({invalid, not_time},
        value({{24, 0, 0}, 0}, {time, nil}, [strict])),

    ?assertEqual({valid, {{0, 0, 0}, 0}},
        value(<<"00:00">>, {time, <<"H:M">>}, [{gt, {23, 59, 59}}])),
    ?assertEqual({valid, {{23, 59, 0}, 0}},
        value(<<"23:59">>, {time, <<"H:M">>}, [{lt, {23, 59, 59}}])),
    ok.

datetime_test() ->
    ?assertEqual({invalid, no_match},
        value(<<"1">>, {datetime, <<"Y">>})),
    ?assertEqual({invalid, no_match},
        value(<<"1">>, {datetime, <<"H">>})),
    ?assertEqual({invalid, not_datetime},
        value(<<"2012-13-24">>, {datetime, <<"Y-m-d">>})),

    ?assertEqual({valid, {{2012, 12, 24}, {{0, 0, 0}, 0}}},
        value(<<"24th Dec 2012 at 00:00">>,
            {datetime, <<"do b Y \\a\\t H:M">>})),
    ok.

boolean_test() ->
    ?assertEqual({valid, true},
        value(<<"1">>, boolean)),
    ?assertEqual({valid, false},
        value(<<>>, boolean)),
    ?assertEqual({valid, false},
        value(<<>>, boolean, [{required, false}])),
    ?assertEqual({valid, false},
        value(0, boolean)),
    ?assertEqual({valid, false},
        value(false, boolean)),
    ?assertEqual({valid, true},
        value(true, boolean)),
    ?assertEqual({valid, false},
        value(false, boolean, [{required, false}])),

    ?assertEqual({invalid, {not_equal, true}},
        value(false, boolean, [{eq, true}])),
    ?assertEqual({valid, false},
        value(false, boolean, [{eq, false}])),

    ?assertEqual({valid, false},
        value(<<"lol">>, boolean, [{false, [<<"lol">>]}])),
    ?assertEqual({valid, true},
        value(<<"">>, boolean, [{false, [<<"lol">>]}])),
    ok.

custom_test() ->
    ?assertEqual({valid, 1},
        value(<<"1">>, integer, [{fun(V, _) -> {valid, V} end, []}])),
    ?assertEqual({valid, 1},
        value(<<"1">>, integer, [{fun is_equal_to/2, [1]}])),
    ?assertEqual({invalid, not_equal},
        value(<<"1">>, integer, [{fun is_equal_to/2, [2]}])),
    ok.

%% Misc checks

in_test() ->
    ?assertEqual({valid, <<"1">>},
        value(<<"1">>, string, [{in, [<<"1">>, <<"2">>]}])),
    ?assertEqual({valid, 1},
        value(<<"1">>, integer, [{in, [1, 2]}])),
    ?assertEqual({invalid, not_in_list},
        value(<<"1">>, string, [{in, [1, 2]}])),
    ok.

key_in_test() ->
    ?assertEqual({valid, <<"1">>},
        value(<<"1">>, string, [{key_in, [{<<"1">>, b}, {<<"2">>, c}]}])),
    ?assertEqual({invalid, not_in_list},
        value(<<"1">>, string, [{key_in, [{1, a}, {2, b}]}])),
    ok.

%% Proplists

proplist_test() ->
    Rules = [
        {<<"age">>, integer, [required, {range, 12, 32}]},
        {<<"gender">>, string, [required, {min_len, 1}, {max_len, 1},
                {in, [<<"m">>, <<"f">>]}]}
    ],
    ?assertEqual({errors, [{<<"gender">>, empty}, {<<"age">>, {less_than, 12}}]},
        proplist(Rules, [{<<"age">>, <<"10">>}])),
    ?assertEqual({ok, [{<<"gender">>, <<"m">>}, {<<"age">>, 32}]},
        proplist(Rules, [{<<"age">>, <<"32">>}, {<<"gender">>, <<"m">>}])),

    Rules2 = [
        {<<"name">>, string, [{range, 3, 16}]},
        {<<"email">>, string, [email, {range, 3, 16}]},
        {<<"age">>, integer, []},
        {<<"date">>, {date, <<"Y-m-d">>}, []}
    ],
    ?assertEqual({errors, [
                {<<"date">>, no_match}, {<<"age">>, not_integer}]},
        proplist(Rules2, [
            {<<"date">>, <<"test">>},
            {<<"email">>, <<"gs+test@gmail.lol">>},
            {<<"age">>, <<"4jk43">>},
            {<<"name">>, <<"dude">>}
        ])),
    ok.

is_equal_to(A, [B]) when A =:= B -> {valid, A};
is_equal_to(_, _) -> {invalid, not_equal}.
-endif.
