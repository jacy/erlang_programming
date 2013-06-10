%% @author jacy
%% @doc @todo Add description to ch5_metex.


-module(ch5_metex).
-compile(export_all).

start() -> register(?MODULE, spawn(?MODULE, init, [])).

init() -> free().

free() ->
	receive
		{wait, Pid} -> 
			Pid ! ok,
			busy(Pid);
		stop ->
			terminate()
	end.

busy(Pid) ->
	receive
		{signal,Pid} ->
			Pid ! ok,
			free()
	end.

terminate() ->
	receive
		{wait,Pid} ->
			exit(Pid,kill),
			terminate()
	after
		0 -> ok
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================


