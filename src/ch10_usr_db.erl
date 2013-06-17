-module(ch10_usr_db).
-compile(export_all).

-include("ch10_usr.hrl").

%% UsrRam
%% 		A named ETS table storing usr records and used for fast access of the subscriber data
%% UsrIndex
%% 		A named ETS table used to index the subscriber id to the msisdn
%% UsrDisk
%% 		A Dets table mirroring the usrRam table for redundancy and persistency purposes


%% ====================================================================
%% API functions
%% ====================================================================
create_tables(FileName) ->
	ets:new(usrRam, [named_table,{keypos, #usr.msisdn}]),
	ets:new(usrIndex,[named_table]),
	dets:open_file(usrDisk,[{file, FileName}, {keypos, #usr.msisdn}]).

close_tables() ->
	ets:delete(usrRam),
	ets:delete(usrIndex),
	dets:close(usrDisk).

add_usr(#usr{msisdn=PhoneNum, id=CustomId} = Usr) ->
	ets:insert(usrIndex, {CustomId,PhoneNum}),
	update_usr(Usr).

update_usr(Usr) ->
	ets:insert(usrRam, Usr),
	dets:insert(usrDisk, Usr),
	ok.

lookup_id(CustId) ->
	case get_index(CustId) of
		{ok,PhoneNo} -> lookup_msisdn(PhoneNo);
		{error, instance} -> {error, instance}
	end.

lookup_msisdn(PhoneNo) ->
	case ets:lookup(usrRam, PhoneNo) of
		[Usr] -> {ok, Usr};
		[] -> {error, instance}
	end.

get_index(CustId) ->
	case ets:lookup(usrIndex, CustId) of
		[{CustId,PhoneNo}] -> {ok, PhoneNo};
		[] -> {error, instance}
	end.

%% traverse function takes the Dets table name and a fun that is applied to every element
restore_backup() ->
	Insert = fun(#usr{msisdn=PhoneNo, id=Id} = Usr) ->
		ets:insert(usrRam, Usr),
		ets:insert(usrIndex, {Id, PhoneNo}),
		continue
	end,
	dets:traverse(usrDisk, Insert).

%% when traversing tables, we need to lock the tables using the safe_fixtable/2 call, as destructive operations during the traversal 
%% might cause a runtime error, or even worse, an undefined behavior? Using safe_fixtable/2 guarantees that we will traverse each 
%% and every element only once  without being affected by any destructive operations executed after starting the traverse.
delete_disabled() ->
	ets:safe_fixtable(usrRam, true),
	catch loop_delete_disabled(ets:first(usrRam)), %The reason for using a catch, and not a try catch, is that we are not interested in the return value
	ets:safe_fixtable(usrRam, false),
	ok.

loop_delete_disabled('$end_of_table') -> ok;
loop_delete_disabled(PhoneNo) ->
	case ets:lookup(usrRam, PhoneNo) of
		[#usr{status=disabled, id = CustId}] -> delete_usr(PhoneNo, CustId);
_ 		-> ok
	end,
	loop_delete_disabled(ets:next(usrRam, PhoneNo)).
	
delete_usr(PhoneNo, CustId) ->
	ets:delete(usrRam, PhoneNo),
	ets:delete(usrIndex, CustId),
	dets:delete(usrDisk, PhoneNo).
%% ====================================================================
%% Internal functions
%% ====================================================================


