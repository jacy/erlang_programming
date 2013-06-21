-module(eb_sup).
-behavior(supervisor).
-export([init/1]). % callback
-export([start_link/0]).

%% A supervisor is a process which monitors what are called child processes. 
%% If a child process goes down, it uses that child’s restart strategy to restart the process. 
%% restart strategies:
%% 	one_for_one - When one of the child processes dies, the supervisor restarts it. Other child processes aren’t affected.
%% 	one_for_all - When one of the child processes dies, all the other child processes are terminated, and then all restarted.
%% 	rest_for_one - When one of the child processes dies, the “rest” of the child processes defined after it in the child specification list are terminated, then all restarted.
%% 
%%	{RestartStrategy, MaxRetries, MaxTime}
%%  If a child process is restarted more than MaxRetries times in MaxTime seconds, then the supervisor terminates all child processes and then itself. 
%% 	This is to avoid an infinite loop of restarting a child process.

%% ====================================================================
%% Callback
%% ====================================================================
%% Id -> is only used internally by the supervisor to store the child specification, but its a general convention to have the ID be the same as the module name 
%% of the child process unless you’re starting multiple instances of your module, in that case suffix the ID with the number.
%% 
%% StartFunc -> is a tuple in the format of {Module, Function, Args} which specifies the function to call to start the process. REALLY IMPORTANT: The start function 
%% must create and link to the process, and should return {ok, Pid}, {ok, Pid, Other}, or {error, Reason}. The normal OTP start_link methods follow this rule. 
%% But if you implement a module which starts its own custom processes, make sure you use spawn_link to start them (hence the blog title, if you didn’t know).

%% Shutdown tells the supervisor how to terminate child processes. The atom “brutal_kill” shuts the child process down without calling the terminate method. 
%% Any integer greater than zero represents a timeout for a graceful shutdown. And the atom “infinity” will gracefully shutdown the process and wait forever for it to stop.

%% Modules is either a list of modules this process affects or the atom “dynamic.” 95% of the time, 
%% you will just use the single OTP callback module in a list for this value. You use “dynamic” if the process is a gen_event process, 
%% since the modules it affects are dynamic 

-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent % process is always restarted
				   | transient % process is never restarted
				   | temporary,% process is only restarted if it terminated abnormally
	Modules :: [module()] | dynamic.
init([]) ->
	Shutdown = 2000,
	EventManager = {eb_event_manager,
				  {eb_event_manager, start_link,[]},
            	  permanent,Shutdown,worker,dynamic}, % use “dynamic” if the process is a gen_event process, since the modules it affects are dynamic 
	Server = {eb_server, {eb_server, start_link, []},
            permanent,Shutdown,worker,[eb_server]},
  	ATM = {eb_atm, {eb_atm, start_link, []},
         permanent,Shutdown,worker,[eb_atm]},
  	{ok,{{one_for_one,5,10}, [EventManager, Server, ATM]}}.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

