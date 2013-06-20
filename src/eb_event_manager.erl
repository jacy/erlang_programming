-module(eb_event_manager).

%% API
-export([start_link/0, add_handler/1, notify/1]).
-define(SERVER, ?MODULE).

%%====================================================================
%% Event Manager
%%====================================================================
%% gen_event works by running an event manager process, with many handler processes running along-side it. 
%% The event manager receives events from other processes, and each handler in turn is notified about the events, and can do what it pleases with them.
add_handler(Module) ->
  gen_event:add_handler(?SERVER, Module, []).

notify(Event) ->
  gen_event:notify(?SERVER, Event). % asynchronous request

start_link() ->
  gen_event:start_link({local, ?SERVER}). 