-module(emysql_transactions).

-compile(export_all).

-include_lib("emysql/include/emysql.hrl").
%% 
%% @doc entry point for transaction handler
%%
transaction( List ) ->
	?tdb("~p:transaction( ~n~p )", [?MODULE, List]),
	transaction(List, 0).

%%
%% @doc emysql:execute_transaction process spawner
%%
transaction( [], Ct) ->
	transaction(Ct, 0, 0, []);

%
% Args is just a term() that we pass around to the pool process.
%
transaction( [ Parms | Rest ], Ct ) ->
	spawn_link( emysql, execute_transaction, [self()|Parms] ),
	transaction( Rest, Ct+1).


%% 
%% @doc transaction response handler
%%

% everyting ok
transaction( 0, _, 0, Pids) ->
	lists:map( fun(P) -> P ! {self(), commit } end, Pids);

% at least one bad
transaction( 0, _, Bads, Pids) ->
	?tdb("~p actions went bad, we're doing a rollback ~n", [Bads]),
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
			?tdb("transaction received unexpected: ~p", [Other])
	after 1000 ->
		?tdb("Aborting all transaction actions due to a timeout", []),
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
				?tdb("Args: ~p action eof ~p", [Args, M]),
				true;
			#error_packet{msg=M} ->
				?tdb("Args: ~p action error ~p", [Args, M]),
				true;
			_ -> false
		end
	end, Results).
	
