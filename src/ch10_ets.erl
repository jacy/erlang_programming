-module(ch10_ets).
-compile(export_all).
-include_lib("stdlib/include/ms_transform.hrl").
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
%% ====================================================================
%% Select Table Elements
%% ====================================================================
%% match specification is a list of 3-tuples, each corresponding roughly to a function clause.
%% let's took ets:select(countries, [{{'$1','$2','$3'},[{'/=','$3',cook}],[['$2','$1']]}]) for example, here are three parts:
%% 1. {'$1','$2','$3'} -> This is a pattern, the same as that used earlier in the ets:match function.
%% 2. [{'/=','$3',cook}] -> This is a list of guard expressions, written in prefix form. The single guard here checks the condition that $3 /= cook. 
%%	  The match is successful only if each guard evaluates to true.
%% 3. [['$2','$1']] ->This is the return expression.
select_demo() ->
	TabId = init(),
	add(TabId, {jacy, "Spinach", 16}),
	add(TabId, {lina, "Cucumber", 18}),
	add(TabId, {lilei, "Celery", 16}),
	
	myio:p(ets:select(TabId,[
							 {
							 	{'$1', '$20', '$300'}, % Name the first,second,third element so that can be reference in the following match
							 	[{'/=', '$300', 16}], % Guards that third element /= 16
							 	[['$20','$1']]  %Return second and first element
							 }
							])).
	
%% ====================================================================
%% fun2ms
%% ====================================================================
%% As the syntax of ets:select/2 is cumbersome, support in Erlang for describing match specifications with closures has been implemented.
%% The function ets:fun2ms/1 takes a fun as an argument, describing the comparison together with the return values we want the select to return.
%% And fortunately for us, fun2ms returns a match specification we can use as an argument in our select call, relieving us of the need to understand or write match specifications:
fun2ms_demo() ->
	TabId = init(),
	add(TabId, {jacy, "Spinach", 16}),
	add(TabId, {lina, "Cucumber", 18}),
	add(TabId, {lilei, "Celery", 16}),
	
%% 	Note that the fun has to be a literal function, and it cannot be passed to fun2ms as a variable. By literal function, we mean a function that is typed in the ets:fun2ms/1 
%%  call and not one that is bound to a variable. The fun itself needs to have one argument, which will be a tuple. Finally, if this is used in a module, a header file needs to be included:
%% -include_lib("stdlib/include/ms_transform.hrl").
	Ms = ets:fun2ms(fun({Name, Description, Age}) when Age /=16 -> [Description, Name] end), % Return [{{'$1','$2','$3'},[{'/=','$3',16}],[['$2','$1']]}]
	myio:p(ets:select(TabId,Ms)).
%% ====================================================================
%% Records and ETS Tables
%% ====================================================================
%% Remember that the default key position in ETS tables is the first element of the tuple. In records, that position is reserved for the record type; 
%% unless you explicitly state the key position, you will not get the intended behavior.
-record(capital, {name, country, pop}).
records_demo() ->
	ets:new(countries, [named_table, {keypos, #capital.name}]), % specify name as key
	ets:insert(countries, #capital{name="Budapest", country="Hungary",pop=2400000}),
	ets:insert(countries, #capital{name="Pretoria", country="South Africa", pop=2400000}),
	ets:insert(countries, #capital{name="Rome", country="Italy",pop=5500000}),
	myio:p(ets:lookup(countries, "Pretoria")),
	myio:p(ets:match(countries, #capital{name='$1',country='$2', _='_'})),
	myio:p(ets:match_object(countries, #capital{country="Italy", _='_'})),
	MS = ets:fun2ms(fun(#capital{pop=P, name=N}) when P < 5000000 -> N end),
	myio:p(ets:select(countries, MS)).
%% ====================================================================
%% Visualizing Tables
%% ====================================================================
%% The Erlang system comes with a tool for visualizing the current state of ETS and Mnesia tables; tables owned by both the current node and connected nodes are 
%% shown when the visualizer is launched by calling tv:start().


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







