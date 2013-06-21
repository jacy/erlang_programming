-module(eb_atm).
-behaviour(gen_fsm).
-export([start_link/0,authorize/2,deposit/1,cancel/0]).% API
-export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
		 authorized/2,authorized/3, unauthorized/3, thank_you/2,thank_you/3]). % gen_fsm callbacks
-define(SERVER, ?MODULE).

%% A gen_fsm starts in a state, and any calls/casts made to it are received by a special callback method which is 
%% the named the same as the state the gen_fsm module is in. Based on an action, the module can change states. 
%% 1. init/1 - Initializers the server. Almost identical to gen_server.
%% 2. StateName/2 - In this case, StateName actually will be replaced with a state name. This is called when a message is sent to the server;
%% 	  an action occurs on the finite state machine. This is an asynchronous callback.
%% 3. handle_event/3 - Similar to StateName/2, except that this is sent no matter what state you’re in, when the client calls gen_fsm:send_all_state_event.
%% 	  Again, this is asynchronous.
%% 4. StateName/3 - Equivalent to StateName/2 except this is the synchronous version. The client waits for a response from the server before continuing.
%% 5. handle_sync_event/4 - Equivalent to handle_event/3 except this is the synchronous version.
%% 6. handle_info/3 - Equivalent to gen_server’s handle_info. This receives all messages which weren’t sending using a standard gen_fsm command. 
%% 	  This can include timeout messages, process exit messages, or any messages sent manually with the “!” symbol to the server process.
%% 7. terminate/3 - Called when the server is terminating so you can clean up any resources.
%% 8. code_change/4 - Called when a real-time system upgrade of the server is occuring.
%%====================================================================
%% API
%%====================================================================
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% The sync_send_event method is equivalent to the call method of gen_server. 
%% It sends the message to the current state of the server. 
authorize(Name, PIN) ->
	gen_fsm:sync_send_event(?SERVER, {authorize,Name, PIN}).

deposit(Amount) ->
  gen_fsm:send_event(?SERVER, {deposit, Amount}).

%%  Cancels the ATM transaction no matter what state.
cancel() ->
  gen_fsm:send_all_state_event(?SERVER, cancel).

%%====================================================================
%% Callbacks
%%====================================================================
init([]) ->
  {ok, unauthorized, nobody}. % current state is unauthorized

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Asynchronize State
%%--------------------------------------------------------------------
authorized({deposit, Amount}, State) -> 
	eb_server:deposit(State, Amount),
  	{next_state, thank_you, State, 5000};
authorized(_Event, State) ->
  {next_state, authorized, State}.

thank_you(timeout, _State) ->
  {next_state, unauthorized, nobody};
thank_you(_Event, _State) ->
  {next_state, unauthorized, nobody}.

handle_event(cancel, _StateName, _State) ->
  {next_state, unauthorized, nobody};
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Synchronize State
%%--------------------------------------------------------------------
unauthorized({authorize, Name, Pin}, _From, State) ->
  case eb_server:authorize(Name, Pin) of
    ok ->
      {reply, ok, authorized, Name}; % convert to authorized state
    {error, Reason} ->
      {reply, {error, Reason}, unauthorized, State} % stay at unauthorized state
  end;
unauthorized(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, unauthorized, State}.

authorized({withdraw, Amount}, _From, State) ->
  case eb_server:withdraw(State, Amount) of
    {ok, Balance} ->
      {reply, {ok, Balance}, thank_you, State, 5000};
    {error, Reason} ->
      {reply, {error, Reason}, authorized, State}
  end;
authorized(_Msg, _From, State) ->
  {reply, {error, invalid_message}, authorized, State}.

thank_you(_Msg, _From, State) ->
  {reply, {error, invalid_message}, unauthorized, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.
