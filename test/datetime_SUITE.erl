%% @doc Erlang representation of MySQL date and time values.
%%
%% This test suite illustrates some corner cases for how Emysql represent these
%% values.
-module(datetime_SUITE).

-compile(export_all).
-include("../include/emysql.hrl").

all() ->
    [normal_time, large_time, negative_time, microseconds, datetime,
     encode_time].

init_per_suite(Config) ->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(test_pool, [
        {size, 1},
        {user, test_helper:test_u()},
        {password, test_helper:test_p()},
        {host, "localhost"},
        {port, 3306},
        {database, "hello_database"},
        {encoding, utf8}]),
    Config.

end_per_suite(_Config) ->
    emysql:remove_pool(test_pool),
    ok.

normal_time(_Config) ->
    #result_packet{rows = [[{time, {23, 59, 57}}]]} =
        emysql:execute(test_pool, <<"SELECT TIME '23:59:57'">>).

large_time(_Config) ->
    #result_packet{rows = [[{time, {130, 59, 57}}]]} =
        emysql:execute(test_pool, <<"SELECT TIME '130:59:57'">>).

negative_time(_Config) ->
    %% The semantics of a negative MySQL TIME value is that the whole TIME
    %% value should be negated. We just negate the hours part.
    #result_packet{rows = [[{time, {-35, 59, 57}}]]} =
        emysql:execute(test_pool, <<"SELECT TIME '-35:59:57'">>),

    %% To illustrate what this means, we subtract a second from 00:00:00.
    %% Only the hours part which is zero gets negated. As you see, it doesn't
    %% make sense.
    #result_packet{rows = [[{time, {0, 0, 1}}]]} =
        emysql:execute(test_pool,
                       <<"SELECT TIME '00:00:00' - INTERVAL 1 SECOND">>).

microseconds(_Config) ->
    %% Truncate microseconds in TIME values.
    #result_packet{rows=[[{time, {23, 59, 57}}]]} =
        emysql:execute(test_pool, <<"SELECT TIME '23:59:57.654321'">>),

    %% Truncate microseconds in DATETIME values.
    #result_packet{rows=[[{datetime, {{2014, 11, 22}, {23, 59, 57}}}]]} =
        emysql:execute(test_pool,
                       <<"SELECT TIMESTAMP '2014-11-22 23:59:57.654321'">>).

datetime(_Config) ->
    %% Fetch a datetime
    #result_packet{rows = [[{datetime, {{2014, 11, 22}, {23, 59, 57}}}]]} =
        emysql:execute(test_pool,
                       <<"SELECT CAST('2014-11-22 23:59:57' AS DATETIME)">>).

encode_time(_Config) ->
    %% This testcase illustrate how time values are encoded when sent to the
    %% server.

    %% Here, we send the time 10:11:12 which is wrongly interpreted as
    %% a DATE and when we add a minute to it, we get a DATETIME.
    %% Somehow we get it as a binary instead of a datetime tuple. (Why?)
    %% {datetime, {{2010, 11, 12}, {0, 1, 0}}}
    #result_packet{rows = [[<<"2010-11-12 00:01:00">>]]} =
        emysql:execute(test_pool,
                       <<"SELECT ? + INTERVAL 1 minute">>,
                       [{time, {10, 11, 12}}]),

    %% In the common case, the value is used in a TIME context, e.g. inserted
    %% or updated in a column of type TIME. In this case it works as expected.
    #result_packet{rows = [[{time, {10, 11, 12}}]]} =
        emysql:execute(test_pool,
                       <<"SELECT CAST(? AS TIME)">>,
                       [{time, {10, 11, 12}}]),

    %% This is to illustrate that what we actually send is a binary.
    #result_packet{rows = [[<<"101112">>]]} =
        emysql:execute(test_pool,
                       <<"SELECT ?">>,
                       [{time, {10, 11, 12}}]).
