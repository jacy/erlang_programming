-module(eb_withdrawal_handler).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).% callback
-export([change_threshold/1]).% API
-define(SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================

change_threshold(Amount) ->
  gen_event:call(eb_event_manager, ?MODULE, {change_threshold, Amount}).

%%====================================================================
%% Callback
%%====================================================================
%%--------------------------------------------------------------------
%% 1. init/1 - Initializes the handler.
%% 2. handle_event/2 - Handles any events sent to the notification manager it is listening to.
%% 3. handle_call/2 - Handles an event which is sent as a call to the notification manager. 
%% 	  A call in this context is the same as a gen_server call: it blocks until a response is sent.
%% 4. handle_info/2 - Handles any non-event and non-call messages sent to the handler.
%% 5. terminate/2 - Called when the handler is quitting so the process can clean up any open resources.
%% 6. code_change/3 - Called when the module is experiencing a real-time system upgrade. This will not be covered in this article, but it will play a central role of a future article in this series.
%%--------------------------------------------------------------------
init([]) ->
	MinWithdrawAmoutToNotice = 500,
	{ok, MinWithdrawAmoutToNotice}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
%%--------------------------------------------------------------------
%% Synchronize
%%--------------------------------------------------------------------
handle_call({change_threshold, Amount}, State) ->
  io:format("NOTICE: Changing withdrawal threshold from ~p to ~p~n", [State, Amount]),
  {ok, {ok, State, Amount}, Amount};
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Asynchronize
%%--------------------------------------------------------------------
handle_event({withdraw, Name, Amount, NewBalance}, State) when Amount >= State -> % Get events if only the amout withdrawn is above MinWithdrawAmoutToNotice
  io:format("WITHDRAWAL NOTIFICATION: ~p withdrew ~p leaving ~p left.~n", [Name, Amount, NewBalance]),
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.