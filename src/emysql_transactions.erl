-module(emysql_transactions).

-include_lib("emysql/include/emysql.hrl").

-export( [transaction/3, transaction/4,
	parallel_transaction_handler/3, parallel_transaction_handler/4] ).


transaction( Type, Sequential, Queries) ->
	transaction( Type, Sequential, Queries, emysql:default_timeout() ).

%% @doc do a list of queries in parallel, and only commit them when ALL of
%% them have no mysql errors.
%%
%% TODO: implement sequential 
%%
-spec transaction( blocking|nonblocking, parallel|sequential, 
	[ { #pool{}, string() | binary(), term() } ], pos_integer() ) -> 
	ok |
	{atomic, [ [#ok_packet{} | #result_packet{} ] ]}  |
	{error,  [ [#error_packet{} | #ok_packet{} | #result_packet{} ] ] } |
	{error, timeout}.


transaction( blocking, parallel, Queries, Timeout) ->

	Pid = spawn_link( ?MODULE, parallel_transaction_handler, [self(), Timeout, Queries] ),

	receive 
		{Pid, error, Results} ->
			{error, Results};
		{Pid, _Responses }  -> 
			{atomic, _Responses}
	after
		Timeout ->
			{error, timeout}
	end;

transaction( nonblocking, parallel, Queries, Timeout) ->
	spawn( ?MODULE, parallel_transaction_handler, [undefined, Timeout,
	Queries] ),
	ok.
		
		

%% 
%% @doc entry point for transaction handler
%%
parallel_transaction_handler( OwnerPid, Timeout, List ) ->
	parallel_transaction_handler(OwnerPid, Timeout, List, 0).


%%
%% @doc emysql:execute_transaction process spawner
%%
parallel_transaction_handler(OwnerPid,  Timeout, [], Ct) ->
	parallel_transaction_handler(OwnerPid, Ct, 0, 0, [], [], Timeout);

%
% Args is just a term() that we pass around to the pool process.
%
parallel_transaction_handler( OwnerPid, Timeout, [ Parms | Rest ], Ct ) ->
	_Pid = spawn_link( emysql, execute_transaction, [self()|Parms] ),
	?tdb("spawned ~p ~p", [_Pid, [self()|Parms]]),
	parallel_transaction_handler( OwnerPid, Timeout, Rest, Ct+1).




finish_workers( Pids, Message) 
	when Message =:= commit orelse Message =:= rollback ->
	lists:map( fun(P) -> 
		P ! {self(), Message },
		receive
			{P, done} -> ok
		after
			100 -> 
				ct:print("too late", [])
		end
	end, Pids).



%% 
%% @doc transaction response handler
%%

% everyting ok
parallel_transaction_handler( OwnerPid, 0, _, 0, Pids, Responses, _Timeout) ->
	finish_workers(Pids, commit),
	case OwnerPid of
		undefined -> 
			ok;
		OwnerPid ->
			OwnerPid ! {self(), Responses}
	end;
			

% at least one bad
parallel_transaction_handler( OwnerPid, 0, _, _Bads, Pids, Responses, _Timeout) ->
	?tdb("~p actions went bad, we're doing a rollback ~n", [_Bads]),
	finish_workers(Pids, rollback),
	case OwnerPid of
		undefined ->
			ok;
		OwnerPid ->
			OwnerPid ! {self(), error, Responses}
	
	end;
% still handling responses from transaction pool processes.
parallel_transaction_handler( OwnerPid, Remaining, Goods, Bads, Pids,
Responses, Timeout) ->
	?tdb("parallel_transaction_handler(~p,~p,~p,~p,~p,~p)", [
		OwnerPid, Remaining, Goods, Bads, Pids, Responses
	]),
	receive
		{Pid, {Results, Args}= Re} ->
			case is_not_ok(Results, Args) of 
				true  -> parallel_transaction_handler(OwnerPid, Remaining-1, Goods, Bads+1, 
					[Pid|Pids], [Re|Responses] , Timeout);
				false -> parallel_transaction_handler(OwnerPid, Remaining-1, Goods+1,
					Bads, [Pid|Pids], [Re|Responses], Timeout )
			end;
		_Other ->
			?tdb("parallel_transaction_handler received unexpected: ~p", [_Other])
	after Timeout ->
		ct:print("Aborting all transaction actions due to a timeout", []),
		lists:map( fun(P) -> P ! {self(), rollback } end, Pids)
	end.

%%
%% @doc check if a list of responses from emysql_tcp:send_and_recv_packet/3
%% contains bad responses.
%%
-spec is_not_ok( [tuple()], [term()] ) -> boolean().

is_not_ok( Results , _Args) ->
	lists:any( fun(R) ->
		case R of
			#eof_packet{status=_M} ->
				?tdb("Args: ~p action eof ~p", [_Args, _M]),
				true;
			#error_packet{msg=_M} ->
				?tdb("Args: ~p action error ~p", [_Args, _M]),
				true;
			_ -> false
		end
	end, Results).
	
