-module(race9).
-export([run/0]).

-include_lib("../../include/emysql.hrl").

run() ->

    crypto:start(),
    application:start(emysql),
    application:set_env(emysql, lock_timeout, infinity),

    emysql:add_pool(hello_pool, 30,
        "hello_username", "hello_password", "localhost", 3306,
        "hello_database", utf8),

    emysql:execute(hello_pool, <<"delete from hello_table">>),

    AmountWriters = 1000,
    AmountCycles = 100,
    Expect = AmountWriters * AmountCycles,

    io:format("race9 test: starting...~n", []),
    [ wait_pids(create_writers(AmountWriters), normal, 5000) || _ <- lists:seq(1, AmountCycles) ],
    io:format("race9 test: finished~n", []),

    Result = emysql:execute(hello_pool,
        <<"select count(hello_text) from hello_table">>),

    [[Val]] = Result#result_packet.rows,
    io:format("total records: ~p~n", [Val]),
    io:format("expected total records: ~p~n", [Expect]),
    
    case Val of
        Expect -> io:format("ok", []);
        _ -> io:format("~p QUERIES LOST.~n", [Expect - Val])
    end.
    

create_writers(N) ->
    lists:foldl( fun(X, Set) -> sets:add_element(create_writer(X), Set) end,
                 sets:new(),
                 lists:seq(1, N)
               ).

create_writer(N) ->
    spawn_link( fun() ->
                    Bin = list_to_binary(integer_to_list(N)),
                    Query = <<"INSERT INTO hello_table SET hello_text = 'Hello World!+", Bin/binary, "'">>,
                    emysql:execute(hello_pool, Query)
                end ).

wait_pids(Pids, ExpectedReason, Timeout) ->
    case sets:size(Pids) of
        0 -> ok;
        _ ->
            receive
                {'EXIT', Pid, ExpectedReason} ->
                    case sets:is_element(Pid, Pids) of
                        true ->
%                             io:format("~p exits with reason: ~p~n", [Pid, ExpectedReason]),
                            wait_pids( sets:del_element(Pid, Pids), ExpectedReason, Timeout );
                        false ->
                            wait_pids( Pids, ExpectedReason, Timeout )
                    end;
                {'EXIT', Pid, Reason} ->
                    case sets:is_element(Pid, Pids) of
                        true ->
                            io:format("~p exits with unexpected reason: ~p~n", [Pid, Reason]),
                            {error, wrong_reason, Pid, Reason};
                        false ->
                            wait_pids( Pids, ExpectedReason, Timeout )
                    end
            after Timeout ->
                io:format("timeout for waiting pids: ~p~n", [sets:to_list(Pids)]),
                {error, timeout}
            end
    end.