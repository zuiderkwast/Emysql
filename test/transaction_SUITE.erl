%%%-------------------------------------------------------------------
%%% File     : Emysql/test/transaction_SUITE.erl
%%% Descr    : Suite #7 - Testing connection manager. 
%%% Authors  : B. v. Deenen, H. Diedrich
%%% Created  : 04/18/2012 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run from Emysql/:
%%%     make test-transactions
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-module(transaction_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("../include/emysql.hrl").


% List of test cases.
% Test cases have self explanatory names.
%%--------------------------------------------------------------------
all() -> 
    [ tx_1, tx_2, tx_3, tx_4 ].


%%--------------------------------------------------------------------
init_per_suite(Config) ->

	% if this fails, focus on environment_SUITE to fix test setup.
    crypto:start(),
    application:start(emysql),
 
	emysql:add_pool(uniq_db, 1,
        "hello_username", "hello_password", "localhost", 3306,
        "hello_database", utf8),
        
	emysql:add_pool(nonuniq_db, 1,
        "hello_username", "hello_password", "localhost", 3306,
        "hello_database", utf8),

    Config.
    
% clean up
%%--------------------------------------------------------------------
end_per_suite(_) ->
	ok.


%% Test Case 1
%%--------------------------------------------------------------------
tx_1(_) ->

	emysql:execute(uniq_db, <<"truncate uniq">>),
	emysql:execute(nonuniq_db, <<"truncate nonuniq">>),

	emysql:execute(uniq_db, <<"insert into uniq values (1,1), (2,2)">>),
	emysql:execute(nonuniq_db, <<"insert into nonuniq values (1,1), (2,2)">>),

	check([[1,1],[2,2]],[[1,1],[2,2]]).

%% Test Case 2
%%--------------------------------------------------------------------
tx_2(_) ->

	tx_1(undefined),

	% create a transient transaction process that spawns two processes that
	% perform jobs in a transaction.
	%
	spawn_link( emysql_transactions, transaction, [[
		[ uniq_db,    "insert into uniq values (1,1), (2,2)", 1 ],
		[ nonuniq_db, "insert into nonuniq values (1,1), (2,2)", 2 ]
	]]),

	timer:sleep(100),
	check([[1,1],[2,2]],
		[[1,1],[2,2] ]).





%% Test Case 3
%%--------------------------------------------------------------------
tx_3(_) ->

	tx_1(undefined),

	spawn_link( emysql_transactions, transaction, [[
		[  uniq_db, <<"insert into uniq values    (3,3), (4,4)">>, 1 ],
		[  nonuniq_db, <<"insert into nonuniq values (3,3), (4,4)">>, 2 ]
	]]),

	timer:sleep(100),
	check([[1,1],[2,2],[3,3],[4,4]],
	 [[1,1],[2,2],[3,3],[4,4]]).

tx_4(_) ->
	tx_2(undefined),

	% this should rollback, even though the first statement is ok.
	spawn_link( emysql_transactions, transaction, [[
		[  uniq_db, <<"update uniq set `value` = 8 where `key` = 1">>, 1] ,
		[  uniq_db, <<"insert into uniq values (3,3), (4,4)">>, 2 ]
	]]),
	timer:sleep(100),
	check([[1,1],[2,2]],
		[[1,1],[2,2] ]).


check(Check, NCheck) ->
	#result_packet{rows=Ru2} = emysql:execute(uniq_db, <<"select * from uniq">>),
	#result_packet{rows=Rnu2} = emysql:execute(nonuniq_db, <<"select * from nonuniq">>),

	%-% ct:print("Check :    ~p~n"  "uniq db:    ~p~n"  "nonuniq db: ~p~n", [Check, Ru2, Ru2]),

    Check  = Ru2,  %% <- this is the actual test.
    NCheck = Rnu2, %% <- 
 					
    ok.
