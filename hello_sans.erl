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
-export([run/0]).

run() ->

	crypto:start(),
	application:start(emysql),

	io:format("trying to freerid pool hello_pool ... ~n", []),

	io:format("insert ... ~n", []),
	
	emysql:execute(hello_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	io:format("query 1 ... ~n", []),

	Result = emysql:execute(hello_pool,
		<<"select hello_text from hello_table">>),

	io:format("query 2 ... ~n", []),

	Result2 = emysql:execute(hello_pool,
		<<"select hello_text from hello_table">>),

	io:format("delete ... ~n", []),
	
	emysql:execute(hello_pool,
		<<"DELETE FROM hello_table">>),

	io:format("done ... ~n", []),

	io:format("~n~p~n", [Result]).
		