-module(ch15_gs_http_server).
-behavior(gen_server).
-export([init/1,terminate/2,handle_cast/2,code_change/3,handle_call/3]). % Callback
-export([start/0,stop/0,handle_info/2]). % Public API

-define(PORT, 9999).
-define(THIRTY_SECONDS, 30000).
-define(OPTION,[{packet, 0}, {active, false}]).

-record(server,{socket, connections=[]}).

%% ====================================================================
%% Public API
%% ====================================================================
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).
%% ====================================================================
%% Callback
%% ====================================================================
init(_) ->	
	{ok, ListenSocket} = gen_tcp:listen(?PORT, ?OPTION),
	spawn(fun() -> ch15_http_server:listen(ListenSocket, 1) end),
	{ok, #server{socket = ListenSocket}}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_call(_Request, _From, State) ->
		Reply = not_implemented,
		NewState = State,
		{reply,Reply, NewState}.

handle_info(Info, State) -> 
	myio:p("catch unhandle message:", Info), 
	{noreply, State}.

terminate(Reason, State) ->
	case State of
		#server{connections=Conns,socket=S} ->	
			close(S),
			close(Conns);
		_ -> no_need_to_handler
	end,
	io:format("Terminate with Reason:~p, and current State:~p ~n", [Reason,State]).

code_change(_OldVsn, State, _Extra) -> 
	myio:p("code_change"), 
	{ok, State}.
%% ====================================================================
%% Private
%% ====================================================================
close([])-> ok;
close([Socket | Other])-> 
	ch15_http_server:close(Socket),
	close(Other).



