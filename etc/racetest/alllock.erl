-module(alllock).
-export([run/0, testfun/0]).

-include_lib("../../include/emysql.hrl").

run() ->

    crypto:start(),
    application:start(emysql),
    application:set_env(emysql, lock_timeout, infinity),

    emysql:add_pool(hello_pool, 30,
        "hello_username", "hello_password", "localhost", 3306,
        "hello_database", utf8),

    emysql:execute(hello_pool, <<"delete from hello_table">>),

    io:format("stress test: starting...~n", []),
    
    {T, _} = timer:tc( ?MODULE, testfun, [] ),
    io:format("stress test: finished in ~p msec~n", [T/1000]),

    Result = emysql:execute(hello_pool,
        <<"select count(hello_text) from hello_table">>),

    [[Val]] = Result#result_packet.rows,
    io:format("total records: ~p~n", [Val]).

testfun() ->
            [ ok = wait_pids(create_writers(20000), normal, infinity) || _ <- lists:seq(1, 5) ].

create_writers(N) ->
    % io:format("create_writer ~p~n", [N]),
    lists:foldl( fun(_, Set) -> sets:add_element(create_writer(), Set) end,
                 sets:new(),
                 lists:seq(1, N)
               ).

writer() ->
    Query = <<"INSERT INTO hello_table SET hello_text = 'Hello World'">>,
    try emysql:execute(hello_pool, Query) of
        _ -> ok
    catch
        C:E -> io:format("~p: error ~p:~p~n", [self(), C, E]), writer()
    end.

create_writer() ->
    spawn_link( fun() -> writer() end ).

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