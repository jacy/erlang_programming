%% @author jacy
%% @doc @todo Add description to ch5_my_db.


-module(ch5_my_db).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start() ->
	Pid = spawn_link(?MODULE, loop, []),
	Pid.

loop(Database) ->
	receive
		{From, write, Key, Element} -> 

%% ====================================================================
%% Internal functions
%% ====================================================================


