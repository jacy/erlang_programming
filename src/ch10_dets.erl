-module(ch10_dets).
-compile(export_all).

%% ====================================================================
%% Disk Erlang Term Storage (Dets)
%% ====================================================================
%% Dets tables provide efficient file-based Erlang term storage. They are used together with ETS tables when fast access needs to be complemented with persistency. 
%% Dets tables have a similar set of functions to ETS tables, including functions for retrieving, matching, and selecting. 
%% But as the calls involve disk seek and read operations, they will be much slower than their counterparts on ETS tables. In the R13 release, the size of a Dets file 
%% cannot exceed 2 GB. If you need more than 2 GB of data, you have to fragment it into multiple Dets tables.

%% Dets table types include set, bag, and duplicate_bag. To use them, you have to open them using dets:open_file(TableName, Options), 
%% where the Options argument is a list of key value tuples, including the following:
%% 1. {auto_save, Interval}
%% 		Sets the interval at which the table is regularly flushed. Flushing the table means there is no need to repair it if it is not properly closed. 
%% 		Interval is an integer in milliseconds (the default is 180,000, or three minutes). If you do not want your file to flush, use the atom infinity.
%% 2. {file, FileName}
%% 		Is used to override the default name of the table as a filename and provide a location in which to save the Dets file.
%% 3. {repair, Bool}
%% 		States whether the table should be repaired if it was not properly closed. If repair is needed, setting Bool to true will trigger the repair automatically, 
%% 		whereas false will return the tuple {error, need_repair}.
%% 4. {type, TableType}
%% 		Can be set, bag, or duplicate_bag. Ordered sets are currently not supported in Dets tables.

%% Options used to optimize the table include the following:
%% 1. {max_no_slots, Number}
%% 		Will fragment the table accordingly, optimizing table insertion times. The default value is 2 million entries; the maximum value is 32 million.
%% 2. {min_no_slots, Number}
%% 		Will enhance performance if an estimate is accurate. The default value is 256 entries.
%% 3. {ram_file, Bool}
%% 		Will enhance performance if you need to populate the table with lots of elements. It stores the elements in RAM and spools them to file either 
%% 		when you call dets:sync(Name) or when you close the table. The flag is set to false by default.




%% ====================================================================
%% Creating Tables
%% ====================================================================

%% ====================================================================
%% Close Tables
%% ====================================================================
%% Dets tables are closed when the owning process terminates or calls the dets:close(Name) call. If several processes have opened the same table, 
%% the table will remain open until all of the processes have either terminated or explicitly closed the table. Not closing a table prior to terminating 
%% the Erlang runtime system will result in it being repaired the next time it is opened. This can be a time-consuming task depending on the size of the table.













