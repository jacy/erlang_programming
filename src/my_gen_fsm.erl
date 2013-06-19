-module(my_gen_fsm).

%% A gen_fsm starts in a state, and any calls/casts made to it are received by a special callback method which is 
%% the named the same as the state the gen_fsm module is in. Based on an action, the module can change states. 
%% ====================================================================
%% Callback functions
%% ====================================================================

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

%% ====================================================================
%% Internal functions
%% ====================================================================


