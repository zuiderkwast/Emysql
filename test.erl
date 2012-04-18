-module(test).

-compile(export_all).

-include_lib("emysql/include/emysql.hrl").

t() ->
	error_logger:tty(false),
	crypto:start(),
	emysql:stop(),
	emysql:start(),
	emysql:add_pool(uniq_db, 1, "root", "ssp" , "localhost", 3306, "test1", utf8),
	emysql:add_pool(nonuniq_db, 1, "root", "ssp" , "localhost", 3306, "test2", utf8),

	emysql:execute( uniq_db, <<"truncate uniq">>),
	emysql:execute( nonuniq_db, <<"truncate nonuniq">>),

	emysql:execute( uniq_db, <<"insert into uniq values ( 1,1), (2,2)">>),
	emysql:execute( nonuniq_db, <<"insert into nonuniq values ( 1,1), (2,2)">>),

	show_dbs(),

	% create a transient transaction process that spawns two processes that
	% perform jobs in a transaction.
	%
	spawn_link( ?MODULE, transaction, [[
		{ uniq_db,    "insert into uniq values ( 1,1), (2,2)", 1 },
		{ nonuniq_db, "insert into nonuniq values ( 1,1), (2,2)", 2 }
	]]),

	timer:sleep(100),
	show_dbs(),


	spawn_link( ?MODULE, transaction, [[
		{  uniq_db, <<"insert into uniq values    ( 3,3), (4,4)">>, 1 },
		{  nonuniq_db, <<"insert into nonuniq values ( 3,3), (4,4)">>, 2 }
	]]),

	timer:sleep(100),
	show_dbs(),
	ok.

%% 
%% @doc entry point for transaction handler
%%
transaction( List ) ->
	transaction(List, 0).

%%
%% @doc emysql:execute_transaction process spawner
%%
transaction( [], Ct) ->
	transaction(Ct, 0, 0, []);

%
% Args is just a term() that we pass around to the pool process.
%
transaction( [ {Pool, Query, Args} | Rest ], Ct ) ->
	io:format("Args:~p ~p doing ~p in multi-transaction ~n", [Args, Pool, Query]),
	spawn( emysql, execute_transaction, [Pool, Query, self(), Args] ),
	transaction( Rest, Ct+1).


%% 
%% @doc transaction response handler
%%
-spec transaction( 
	Remaining :: non_neg_integer(),   % responses remaining
	Goods :: non_neg_integer()        % count of good responses up to now
	Bads  :: non_neg_integer()        % count of bad responses up to now
	Pids  :: [pid()].                 % pids of pool processes.

% everyting ok
transaction( 0, _, 0, Pids) ->
	lists:map( fun(P) -> P ! {self(), commit } end, Pids);

% at least one bad
transaction( 0, _, Bads, Pids) ->
	io:format("~p actions went bad, we're doing a rollback ~n", [Bads]),
	lists:map( fun(P) -> P ! {self(), rollback } end, Pids);
	
% still handling responses from transaction pool processes.
transaction( Remaining, Goods, Bads, Pids) ->
	receive
		{Pid, {Results, Args}} ->
			case is_not_ok(Results, Args) of 
				true  -> transaction(Remaining-1, Goods, Bads+1, [Pid|Pids]);
				false -> transaction(Remaining-1, Goods+1, Bads, [Pid|Pids])
			end;
		Other ->
			io:format("Other: ~p~n", [Other])
	after 1000 ->
		io:format("Aborting transaction due to timeout"),
		lists:map( fun(P) -> P ! {self(), rollback } end, Pids)
	end.

%%
%% @doc check if a list of responses from emysql_tcp:send_and_recv_packet/3
%% contains bad responses.
%%
-spec is_not_ok( [tuple()], [term()] ) -> boolean().

is_not_ok( Results , Args) ->
	lists:any( fun(R) ->
		case R of
			#eof_packet{status=M} ->
				io:format("Args: ~p action eof ~p~n", [Args, M]),
				true;
			#error_packet{msg=M} ->
				io:format("Args: ~p action error ~p~n", [Args, M]),
				true;
			_ -> false
		end
	end, Results).
	

show_dbs() ->
	io:format("-----------------------------------------------------~n",[]),
	#result_packet{rows=Ru2} = emysql:execute(uniq_db, <<"select * from uniq">>),
	#result_packet{rows=Rnu2} = emysql:execute(nonuniq_db, <<"select * from nonuniq">>),

	io:format("uniq db:    ~p~n", [Ru2]),
	io:format("nonuniq db: ~p~n", [Rnu2]),
	io:format("-----------------------------------------------------~n",[]).
					
					
					

