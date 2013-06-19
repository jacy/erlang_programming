-module(ch5_my_db).
-record(users,{name,password}).
-define(DBNAME,testDb).
-define(SERVER,?MODULE).
-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
	myio:p("Current process id:",self()),
	Pid = spawn_link(?MODULE, init, []),
	register(?SERVER, Pid).

stop() ->
	Ref = make_ref(),
	?SERVER ! {{self(),Ref}, stop},
	receive {Ref, ok} -> ok
	after 
		3000 -> myio:p("Stop Database Server Timeout!")
	end.

write(Name,Password) ->
	Ref = make_ref(),
	?SERVER ! {{self(),Ref}, write, #users{name=Name,password=Password}},
	receive {Ref, ok} -> ok
	after 
		3000 -> myio:p("Write Timeout!")
	end.

read(Name) ->
	Ref = make_ref(),
	?SERVER ! {{self(),Ref}, read, Name},
	receive {Ref,User} -> myio:p("Read Entry:", User)
	after 
		3000 -> myio:p("read Timeout!")
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================
init() ->
	ets:new(?DBNAME, [named_table,{keypos, #users.name}]),
	loop().

loop() ->
	receive
		{{From, Ref}, write, #users{name=_Name,password=_Password} = Entry} -> 
			ets:insert(testDb, Entry),
			myio:p("Add entry:", Entry),
			From ! {Ref, ok},
			loop();
		{{From, Ref}, delete, Key} -> 
			ets:delete(testDb, Key),
			myio:p("Delete entry with key:", Key),
			From ! {Ref, ok},
			loop();
		{{From, Ref}, read, Key} -> 
			Entry = ets:lookup(testDb, Key),
			myio:p("Search key:", Key),
			From ! {Ref, Entry},
			loop();
		{{From, _Ref}, stop} -> 
			ets:delete(?DBNAME),
			myio:p("Stop Database"),
			From ! {_Ref, ok}
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================


