%% @author jacy
%% @doc @todo Add description to ch5_metex.


-module(ch5_metex).
-compile(export_all).

start() -> register(?MODULE, spawn(?MODULE, init, [])).

stop() -> ?MODULE ! stop.


wait() -> ?MODULE ! {wait, self()}, receive ok -> ok end.

signal() -> ?MODULE ! {signal, self()}, ok.

init() -> free().

free() ->
	receive
		{wait, Pid} -> 
			Pid ! ok,
			busy(Pid);
		stop ->
			myio:p("~p is stopping !",self()),
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
			exit(Pid, kill),
			terminate()
	after
		0 -> ok
	end.
