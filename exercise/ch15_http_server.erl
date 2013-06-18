-module(ch15_http_server).
-compile(export_all).
-define(PORT, 9999).
-define(THIRTY_SECONDS, 30000).
-define(OPTION,[{packet, 0}, {active, false}]).

start() -> register(?MODULE, spawn_link(?MODULE, init, [])).

%% stop() -> 
%%  	exit(whereis(?MODULE)),
%%  	unregister(?MODULE).

init() ->
	{ok, ListenSocket} = gen_tcp:listen(?PORT, ?OPTION),
	listen(ListenSocket, 1).

listen(ListenSocket, Count) ->
	{ok, Connection} = gen_tcp:accept(ListenSocket),
	io:format("Accepted pid ~p, count ~p ~n",[self(), Count]),
	_Pid = spawn(?MODULE, pull_request, [Connection, Count]),
%%  gen_tcp:controlling_process(Connection, _Pid), % Why This is optional in this case ?????????????
	listen(ListenSocket, Count + 1).

pull_request(Connection, Count) ->
	case gen_tcp:recv(Connection, 0, ?THIRTY_SECONDS) of
		{ok, Binary} -> 
			io:format("Pull request form pid ~p, count ~p ,with body: ~n ~p ~n",[self(), Count, Binary]),
			response(Binary, Connection),
			close(Connection);
		{error,timeout} -> 
			io:format("Pull Request timeout of count:~p ~n",[Count]),
			close(Connection)
	end.

response(Binary, Connection) ->
	LineBreak = "\r\n",
	StatusLine = "HTTP/1.1 200 OK" ++ LineBreak,
	RespBody = "Test",
	RespHeader = lists:concat(["Content-Length: ",string:len(RespBody), LineBreak]),
	Cookie = "Set-Cookie: JSESSIONID=TODO; expires=Thu, 18-Jun-2015 07:45:30 GMT; path=/;" ++ LineBreak,
	
	Resp = lists:concat([StatusLine,RespHeader,Cookie,LineBreak,RespBody,LineBreak]),
	gen_tcp:send(Connection, Resp).

close(Socket) -> gen_tcp:close(Socket).
