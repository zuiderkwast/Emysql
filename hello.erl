%% Emysql Hello World!
%% See README.md
%% $ mysql [-u<user> -p]
%% mysql> create database hello_database;
%% mysql> use hello_database;
%% mysql> create table hello_table (hello_text char(20));
%% mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';
%% $ make
%% $ erlc hello.erl
%% $ erl -pa ./ebin -s hello run -s init stop -noshell

-module(hello).
-export([run/0, do/0]).

run() ->

	io:format("main: start ... ~n", []),

	crypto:start(),
	application:start(emysql),

	io:format("main: pool ... ~n", []),

	emysql:add_pool(hello_pool, 1,
		"hello_username", "hello_password", "localhost", 3306,
		"hello_database", utf8),

	io:format("main: fork ... ~n", []),

	Pid1 = spawn(fun() -> do() end),
	Mref1 = erlang:monitor(process, Pid1),
	Pid2 = spawn(fun() -> do() end),
	Mref2 = erlang:monitor(process, Pid2),

	io:format("main: wait ... ~n", []),

	receive
	after 20000 -> none
	end.

do() ->

	io:format("process ~p >>> insert ... ~n", [self()]),
	
	emysql:execute(hello_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	% io:format("process ~p >>> query #1 ... ~n", [self()]),

	% Result1 = emysql:execute(hello_pool,
	%	<<"select hello_text from hello_table">>),

	%io:format("process ~p >>> query #2 ... ~n", [self()]),

	% Result2 = emysql:execute(hello_pool,
	%	<<"select hello_text from hello_table">>),

	% io:format("process ~p >>> delete ... ~n", [self()]),
	
	% emysql:execute(hello_pool,
	%	<<"DELETE FROM hello_table">>),

	% io:format("process ~p >>> done ... ~n", [self()]),
 
	% io:format("~n~p~n", [Result1]),
	% io:format("~n~p~n", [Result2]).
		
		ok.
