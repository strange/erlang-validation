# Introduction

Validation library ... in the making.

Long way from done and nearly no documentation - I'd stay away!

## Usage

You can validate single values:

    validate:value(<<"Test">>, string, [{min_length, 3}]).
    {valid,<<"Test">>}

    validate:value(<<"gs@pipsq.com">>, string, [email]).
    {valid,<<"gs@pipsq.com">>}

    validate:value(<<"2012-12-24">>,
        {date, <<"Y-m-d">>}, [{gt, {2012, 12, 30}}]).
    {invalid, {less_than, {2012, 12, 30}}}

    validate:value(<<"abc123">>, string, [{match, "^[a-z]+$"}]).
    {invalid, no_match}

Or proplists:

    Rules = [
        {<<"age">>, integer, [required, {range, 12, 32}]},
        {<<"gender">>, string, [required, {min_len, 1}, {max_len, 1},
                {in, [<<"m">>, <<"f">>]}]}
    ],
    validate:proplist(Rules, [{<<"age">>, <<"10">>}]).
    {errors, [{<<"gender">>, empty}, {<<"age">>, {less_than, 12}}]}

Validations consist of a type and options. The following types and options are
available:

### All validations

* {required, Bool} -> {invalid, empty}
* {trim, Bool} -> do not trim string and binary input
* strict -> minimal type coercion
* {custom, Fun}

#### string (converts to binary)

* {min_length, N} -> {invalid, {less_than, N}}
* {max_length, N} -> {invalid, {greater_than, N}}
* {match, Re} -> {invalid, no_match}
* url -> {invalid, not_url}
* email -> {invalid, not_email}

#### integer

* {min, Min}
* {max, Max}
* {range, Min, Max}

#### decimal (converts to binary)

* {min, Min}
* {max, Max}
* {lt, Value}
* {gt, Value}
* {lte, Value}
* {gte, Value}
* {eq, Value}

#### json -> terms

#### boolean (converts to true or false)

* {false, Values}

#### {date, Format} (converts to {Y, M, D})

* {gt, Date}
* {gte, Date}
* {lt, Date}
* {lte, Date}
* {range, Start, Stop}

#### {time, Format} (converts to {{H, Min, S}, U})

* {gt, Time}
* {gte, Time}
* {lt, Time}
* {lte, Time}
* {range, Start, Stop}

#### {datetime, Format} (converts to {{Y, M, D}, {{H, Min, S}, U}})

* {gt, DateTime}
* {gte, DateTime}
* {lt, DateTime}
* {lte, DateTime}
* {range, Start, Stop}

TODO
====

Besides refactoring, thinking things through and createing a sane API, there
are plans for:

* Support for locale (most notably decimal mark, but also i18n error messages)
* Multiple choices
* Allow multiple date formats.
* Human readable error messages
* Convert JSON Schema to validation rules?
* Support for more Erlang terms (list, atom, type, float) and nested
  validation rules.
