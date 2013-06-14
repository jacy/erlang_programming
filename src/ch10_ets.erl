%% @author jacy
%% @doc @todo Add description to ch10_ets.


-module(ch10_ets).
-compile(export_all).
%% ====================================================================
%% Erlang Term Storage (ETS)
%% ETS tables store tuples, with access to the elements given through a key field in the tuple.
%% The tables are implemented using hash tables and binary trees, with different representations providing different kinds of collections
%% ====================================================================


%% ====================================================================
%% Creating Tables
%% ====================================================================
%% The default setup when an empty list of options is passed to the ets:new/2 function is
%% to create a set, with the key in position 1, and providing protected access to the values
%% of the table. Protected access allows all processes to read the table, but only the owner
%% to write to it. Other options include:
%	1. set, ordered_set, bag, duplicate_bag: Including one of these in the options list creates an ETS table of the specified sort.
%	2. {keypos, Pos}: This creates a table with the key in position Pos.
%	3. public, protected, private: A public table is readable and writable by all processes; 
%	   a private table is readable and writable only by the process that owns the table.
%	4. named_table: The name of the table is statically registered, and can then be used to reference the table in ETS operations.

init() ->
	_TabId = ets:new(database, []). %name_table not specified, so can only access by TableId
%% ====================================================================
%% Delete Tables
%% ====================================================================
%% 1. Delete the tables manually by calling ets:delete(TabId).
%% 2. Table is linked to the process that created it, and if the process terminates, the table is deleted automatically.

destory(TableId) ->
	ets:delete(TableId).

%% ====================================================================
%% Handling Table Elements
%% ====================================================================
%% You insert elements into a table using ets:insert/2 and access them by their key using ets:lookup/2:

add(TabId,Element) ->
	ets:insert(TabId,Element). % Save or Update elements. 

findByKey(TabId, Key) ->
	ets:lookup(TabId,Key).


deleteByKey(TabId, Key) ->
	ets:delete(TabId,Key).

%% ====================================================================
%% Match Table Elements
%% ====================================================================
%% 1 '_', which is a wildcard that will match any value in this position
%% 2 '$0' and '$1' ... '$num' , which are variables that will match any value in this position
%% 3 A value
%% The result of match is to give a list of results, one for each successful match in the table.
match_demo() ->
	TabId = init(),
	add(TabId, {jacy, "Element 1", 16}),
	add(TabId, {lina, "Element 2", 18}),
	add(TabId, {lilei, "Element 3", 16}),
	
	% '$9' is a variable to hold the first element, hold element have to be pattern like '$num'.
	% "Element 1" 
	%  First element will be collected in result, Second Element has to be "Element 1",Third element can be anything but not return in result.
	myio:p(ets:match(TabId,{'$9',"Element 1",'_'})),
	myio:p(ets:match(TabId,{'jacy','_','$2013'})), % collect third element whose first element is jacy.

	Pattern = {'$9',"Element 1",'_'},
	%  It is possible to return the entire tuple matching a pattern using match_object
	myio:p("Before Delete:", ets:match_object(TabId,Pattern)), % Return all elemnts:

	%  Delete the matching objects by means of match_delete:
	ets:match_delete(TabId, Pattern),
	myio:p("After Delete: ", ets:match_object(TabId,Pattern)).
%% You need to use match operations with great care, as they can change the real-time behavior of a system. This is because all match operations
%% are implemented as BIFs, and BIFs are executed atomically; a match operation on a large table can therefore stop other processes from executing
%% until the operation has traversed the whole table.To avoid this problem, it is best to work by table traversal using first and next, as shown earlier.
	
	

%% Demo
demo() ->
	TabId = init(),
	myio:p(findByKey(TabId, jacy)), % Empty result
	add(TabId, {jacy, "Old content"}),
	myio:p(findByKey(TabId, jacy)), % Old element
	myio:p(ets:info(TabId)),
	add(TabId, {jacy, "New content"}),
	myio:p(findByKey(TabId, jacy)), % New element

	deleteByKey(TabId, jacy),  % Delete element
	myio:p(findByKey(TabId, jacy)),
	destory(TabId).







