-module(ch12_usr).
-export([start_link/0, start_link/1, stop/0]). 
-export([init/1, terminate/2, handle_call/3, handle_cast/2]). 
-export([add_usr/3, delete_usr/1, set_service/3, set_status/2,delete_disabled/0, lookup_id/1]). 
-export([lookup_msisdn/1, service_flag/2]). 
-behavior(gen_server).
-include("usr.hrl").

%% ====================================================================
%% Start server
%% ====================================================================
start_link() -> start_link("usrDb").
start_link(FileName) ->
	%% gen_server:start_link(ServerName, CallBackModule, Arguments, Options)
	%% gen_server:start_link(CallBackModule, Arguments, Options)
	%% 1. ServerName Is a tuple of the format{local, Name} or {global, Name}, denoting a local or global Name for the process if it is to be registered. 
	%% 	  If you do not want to register the process and instead reference it using its pid, you omit the argument and use the start_link/3 or start/3 call instead.
	%% 2. Arguments Is a valid Erlang term that is passed to the init/1 callback function. 
	%% 3. Options Is a list that allows you to set the memory management flags fullsweep_after and heapsize, as well as tracing and debugging flags.
	%% 4. CallbackModule Is the name of the module in which the specific callback functions are placed.
	gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

init(FileName) -> 
	usr_db:create_tables(FileName),
	usr_db:restore_backup(),
	{ok, null}.

%% ====================================================================
%% Passing Messages
%% ====================================================================
%% If you want to send a message to your server, you use the following calls:
%% gen_server:cast(Name, Message) 
%% gen_server:call(Name, Message)
%% Name Is either the local registered name of the server or the tuple{global, Name}. It could also be the process identifier of the server.
%% --------------------------------------------------------------------
%% Asynchronous Messages ->  {noreply, NewLoopData｝
%% --------------------------------------------------------------------
%% For asynchronous message requests, you use cast/2. If you’re using a pid, the call will immediately return the atom ‘ok’, regardless of whether the gen_server 
%% to which you are sending the message is alive.These semantics are no different from the standard “Name ! Message” construct,
%% where if the registered process Name does not exist, the calling process terminates.
%% Upon receiving the message, gen_server will call the callback function handle_cast(Message, LoopData) in the callback module. 
%% 1） Message is the argument passed to the cast/2 function, 
%% 2） LoopData is the argument originally returned by the init/1 callback function. 
%% The handle_cast/1 callback function handles the specifics of the message, and upon finishing, it has to return the tuple {noreply, NewLoopData}. 
%% In future calls to the server, the NewLoopData value most recently returned will be passed as an argument when a message is sent to the server.
%% --------------------------------------------------------------------
%% Synchronous Messages -> {reply, Reply, NewLoopData}
%% --------------------------------------------------------------------
%% If you want to send a synchronous message to the server, you use the call/2 function. Upon receiving this message, the process uses the
%% handle_call(Message, From, LoopData) function in the callback module. It contains specific code for the particular server, and having completed, Argument From is a tuple containing a unique message reference and the client process identifier. 
%% it returns the tuple {reply, Reply, NewLoopData}. Only now does the call/3 function synchronously return the value Reply.
%% the atom 'reply' telling the gen_server generic code(gen server module not the call back module) that the second element of the tuple is the Reply to be sent back to the client.
%% If the process you are sending a message to does not exist, regardless of whether it is registered, the process invoking the call function terminates.
%% --------------------------------------------------------------------
%% Timeout Handler
%% --------------------------------------------------------------------
%% The gen_server library module has a number of mechanisms and safeguards built in that function behind the scenes. If your client sends a synchronous message
%% to your server and you do not get a response within five seconds, the process executing the call/2 function is terminated. 
%% You can override this by using the following code: gen_server:call(Name, Message, Timeout) where Timeout is a value in milliseconds or the atom infinity. 
%% --------------------------------------------------------------------
%% Termination calling process if server not exist
%% --------------------------------------------------------------------
%% Other safeguards when using the gen_server:call/2 function include the case of sending a message to a nonexisting server or a server 
%% that crashes before sending its reply. In both cases, the CALLING PROCESS will TERMINATE. 
%% In raw Erlang, sending a message that is never pattern-matched in a receive clause is a bug that  can cause a memory leak.
%% --------------------------------------------------------------------
%% Handle Other Messages
%% --------------------------------------------------------------------
%% The callback function handle_info/2* is called whenever the process receives a message it doesn’t recognize. 
%% These could include “node down” messages from nodes you are monitoring, exit signals from processes you are linked to, 
%% or simply messages sent using the 'Pid ! Msg' construct.
add_usr(PhoneNum, CustId, Plan) when Plan==prepay; Plan==postpay -> 
	gen_server:call(?MODULE, {add_usr, PhoneNum, CustId, Plan}).

delete_usr(CustId) -> 
	gen_server:call(?MODULE, {delete_usr, CustId}).

set_service(CustId, Service, Flag) when Flag==true; Flag==false -> 
	gen_server:call(?MODULE, {set_service, CustId, Service, Flag}).

set_status(CustId, Status) when Status==enabled; Status==disabled-> 
	gen_server:call(?MODULE, {set_status, CustId, Status}).

delete_disabled() -> gen_server:call(?MODULE, delete_disabled).

handle_cast(stop, LoopData) -> {stop, normal, LoopData}.

handle_call({add_usr, PhoneNo, CustId, Plan}, _From, LoopData) -> 
	Reply = usr_db:add_usr(#usr{msisdn=PhoneNo,id=CustId, plan=Plan}),
	{reply, Reply, LoopData};
handle_call({delete_usr, CustId}, _From, LoopData) ->
	Reply = usr_db:delete_usr(CustId),
	{reply, Reply, LoopData};
handle_call({set_service, CustId, Service, Flag}, _From, LoopData) -> 
	Reply = case usr_db:lookup_id(CustId) of
		{ok, Usr} ->
			Services = lists:delete(Service, Usr#usr.services), 
			NewServices = case Flag of
				true -> [Service|Services]; 
				false -> Services
			end,
			usr_db:update_usr(Usr#usr{services=NewServices}); 
		{error, instance} ->
			{error, instance} end,
	{reply, Reply, LoopData};
handle_call({set_status, CustId, Status}, _From, LoopData) -> 
	Reply = case usr_db:lookup_id(CustId) of
		{ok, Usr} -> usr_db:update_usr(Usr#usr{status=Status});
		{error, instance} ->{error, instance} end,
	{reply, Reply, LoopData};
handle_call(delete_disabled, _From, LoopData) -> {reply, usr_db:delete_disabled(), LoopData}.

%% ====================================================================
%% Stopping the Server
%% ====================================================================
%% In handle_call/3 and handle_cast/2 callback functions, instead of returning {reply, Reply, NewLoopData} or {noreply, NewLoopData}, 
%% Return {stop, Reason, Reply, NewLoopData} or {stop, Reason, NewLoopData}, respectively. Something has to trigger this return value, 
%% often a stop message sent to the server. Upon receiving the stop tuple containing the Reason and LoopData, the generic code executes
%% the terminate(Reason, LoopData) callback.
stop() -> gen_server:cast(?MODULE, stop).

terminate(Reason, LoopData) ->
	io:format("Terminate with Reason:~p, and current State:~p ~n", [Reason,LoopData]),
	usr_db:close_tables().

%% ====================================================================
%% Service API
%% ====================================================================
lookup_id(CustId) -> usr_db:lookup_id(CustId).

lookup_msisdn(PhoneNo) -> usr_db:lookup_msisdn(PhoneNo).

service_flag(PhoneNo, Service) ->
	case usr_db:lookup_msisdn(PhoneNo) of
		{ok,#usr{services=Services, status=enabled}} -> lists:member(Service, Services);
		{ok, #usr{status=disabled}} -> {error, disabled};
		{error, Reason} ->{error, Reason} 
	end.






