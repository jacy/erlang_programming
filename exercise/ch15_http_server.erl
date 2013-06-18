-module(ch15_http_server).
-compile(export_all).
-define(PORT, 9999).
-define(OPTION,[{packet, 0}, {active, false}]).

start() ->
	Pid = spawn(?MODULE, init, []),
	register(?MODULE, Pid).

%% stop() -> 
%% 	exit(whereis(?MODULE)),
%% 	unregister(?MODULE).

init() ->
	{ok,ListenSocket} = gen_tcp:listen(?PORT, ?OPTION),
	listen(ListenSocket, [], 1).

%% handle_stop(ListenSocket) ->
%% 	receive 
%% 		stop -> close(ListenSocket);
%% 		_ -> ok
%% 	after 0 -> timeout
%% 	end.
	
listen(ListenSocket, Sessions, Count) ->
	io:format("Looping in count ~p ~n",[Count]),
	{ok, Connection} = gen_tcp:accept(ListenSocket),
	
	Pid = spawn(?MODULE, get_request, [Connection, Count]),
	gen_tcp:controlling_process(Connection, Pid),
	listen(ListenSocket,Sessions, Count + 1).


get_request(Connection, Count) ->
	case gen_tcp:recv(Connection, 0, 5000) of
		{ok, Binary} -> 
			handle(Binary, Connection),
			get_request(Connection, Count);
		{error, closed} -> io:format("Success close Request of count ~p ~n",[Count]);
		{error,timeout} -> 
			io:format("Timeout of count ~p ~n",[Count]),
			close(Connection)
	end.

handle(Binary, Connection) ->
	io:format("Request body ~p ~n",[Binary]),
	LineBreak = "\r\n",
	StatusLine = "HTTP/1.1 200 OK" ++ LineBreak,
	RespBody = "Test",
	RespHeader = lists:concat(["Content-Length: ",string:len(RespBody),LineBreak]),
	Cookie = "Set-Cookie: JSESSIONID=83DF81AA079028B97C46BC93C2613AEF; expires=Thu, 18-Jun-2015 07:45:30 GMT; path=/;" ++ LineBreak,
	
	Resp = lists:concat([StatusLine,RespHeader,Cookie,LineBreak,RespBody,LineBreak]),
	gen_tcp:send(Connection, Resp),
	close(Connection).

close(Socket) -> gen_tcp:close(Socket).
