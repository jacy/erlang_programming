-module(ch5_event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2]).
-export([init/1]).

%% Event types commonly include alarms, equipment state changes, errors, and trace events, just to mention a few.
%% When they are received, one or more actions are applied to each event.

%%  start a generic event manager, registering it with the alias Name. HandlerList is a list of tuples of the form {Handler, Data},
%%  where Handler is the name of the handler callback module and Data is the argument passed to the handler’s init callback function. 
start(Name, HandlerList) ->
	register(Name, spawn(?MODULE, init, [HandlerList])), 
	ok.

init(HandlerList) ->
	loop(initialize(HandlerList)).

initialize([]) -> []; 
initialize([{Handler, Data}| Rest]) ->
	[{Handler, Handler:init(Data)}|initialize(Rest)].

%% terminate all the handlers and stop the event manager process. It will return a list of items of the form {Handler, Data}, 
%% where Data is the return value of the terminate callback function of the individual handlers.
stop(Name) ->
	Name ! {stop, self()},
	receive {reply, Reply} -> Reply end.

terminate([]) -> []; 
terminate([{Handler, Data}|Rest]) -> 
	[{Handler, Handler:terminate(Data)}|terminate(Rest)].

%% add the handler defined in the callback module Handler, passing Data as an argument to the handler’s init callback function.
add_handler(Name, Handler, Data) -> 
	call(Name, {add_handler, Handler, Data}).

loop(State) -> 
	receive
		{request, From, Msg} ->
			{Reply, NewState} = handle_msg(Msg, State), reply(From, Reply),
			loop(NewState);
		{stop, From} ->
			reply(From, terminate(State))
	end.

call(Name, Msg) ->
	Name ! {request, self(), Msg}, receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
	To ! {reply, Msg}.

%% remove the handler defined in the callback module Handler. The handler’s terminate callback function will be called, 
%% and its return value will be the return value of this call. This call returns the tuple {error, instance} if Handler does not exist.
delete_handler(Name, Handler) ->
	call(Name, {delete_handler, Handler}).

%% return the contents of the state variable of the Handler. This call returns the tuple {error, instance} if Handler does not exist.
get_data(Name, Handler) ->
	call(Name, {get_data, Handler}).

%% forward the contents of Event to all the handlers.
send_event(Name, Event) ->
	call(Name, {send_event, Event}).

handle_msg({add_handler, Handler, InitData}, LoopData) -> 
	{ok, [{Handler, Handler:init(InitData)} | LoopData]};

handle_msg({delete_handler, Handler}, LoopData) -> 
	case lists:keysearch(Handler, 1, LoopData) of
		false ->
			{{error, instance}, LoopData};
		{value, {Handler, Data}} ->
			Reply = {data, Handler:terminate(Data)},
			NewLoopData = lists:keydelete(Handler, 1, LoopData), {Reply, NewLoopData}
	end;

handle_msg({get_data, Handler}, LoopData) ->
	case lists:keysearch(Handler, 1, loopData) of
		false -> {{ error, instance}, LoopData };
		{value, {Handler, Data}} -> {{data, Data}, LoopData}
	end;

handle_msg({send_event, Event}, LoopData) ->
	{ok, event(Event, LoopData)}.

event(_Event, []) -> [];
event(Event, [{Handler, Data}|Rest]) ->
	[{Handler, Handler:handle_event(Event, Data)}| event(Event, Rest)].
