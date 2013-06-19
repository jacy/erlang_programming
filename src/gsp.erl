-module(gsp).
-behavior(gen_server).
-export([init/1,terminate/2,handle_cast/2,code_change/3,handle_call/3]). % Callback
-export([start_to_stop/0,stop/0,handle_info/2,start/0,asyn_msg/0, syn_msg/0,demo/0]). % Test API

-record(mark,{asyn = 0, syn = 0}).

%% ====================================================================
%% Test API
%% ====================================================================
start_to_stop() -> 
	process_flag(trap_exit,true),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [stop], []),
	monitor().

demo() ->
	start(),
	asyn_msg(),
	syn_msg().

asyn_msg() ->  [ async_print(X) || X <- lists:seq(1, 5)].

syn_msg() ->  [ sync_print(X) || X <- lists:seq(1, 5)].

start() -> 
	process_flag(trap_exit,true),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).
%% ====================================================================
%% Callback
%% ====================================================================
init([S = stop]) -> 
	myio:p("Gen server started faild, it was stopped manually!"),
	{S, test_stop_while_init};
init(_) ->	{ok, #mark{}}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast({print,Count}, #mark{asyn = A} = State) -> 
	{H,M,S} = time(),
	io:format("++++++Aysn msg:~p, time:~p:~p:~p ~n", [Count,H,M,S]),
	timer:sleep(2000),
	{noreply, State#mark{asyn=A + 1}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_call({print, Count}, _From, #mark{syn=A} = State) -> 
	{H,M,S} = time(),
	io:format("------Sync msg:~p, time:~p:~p:~p ~n", [Count,H,M,S]),
	timer:sleep(2000),
	{reply, ok,State#mark{syn=A + 1}};
handle_call(_Request, _From, State) ->
		Reply = ok,
		NewState = State,
		{reply,Reply, NewState}.

handle_info(Info, State) -> 
	myio:p("catch unhandle message:", Info), 
	{noreply, State}.

terminate(Reason, State) -> io:format("Terminate with Reason:~p, and current State:~p ~n", [Reason,State]).

code_change(_OldVsn, State, _Extra) -> 
	myio:p("code_change"), 
	{ok, State}.
%% ====================================================================
%% Private
%% ====================================================================
monitor() ->
	receive 
		{'EXIT', Pid, Reason} -> io:format("oooooooooooooooooops ~p died because of ~p ooooooooooooooops~n",[Pid, Reason]);
		Msg -> 
			myio:p("Receive Msg: ~p ~n ", Msg),
			monitor() 
	end.

async_print(Count) -> 
	myio:p("Before async msg:", Count),
	gen_server:cast(?MODULE, {print,Count}),
	myio:p("After async msg:", Count).

sync_print(Count) -> 
	myio:p("Before sync msg:", Count),
	gen_server:call(?MODULE, {print,Count}, 300000),
	myio:p("After sync msg:", Count).