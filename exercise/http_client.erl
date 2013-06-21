-module(http_client).
-define(OPTION,[{packet, 0}, {active, false}]).
-compile(export_all).

baidu() ->
	http_get("www.baidu.com", "/").

http_get(Host, Path) ->
	spawn(fun() -> doget(Host, Path) end).

doget(Host, Path) -> 
	{ok, Socket} = gen_tcp:connect(Host, 80, ?OPTION),
	Headers = "Connection: Close",
	Request = ["GET ", Path, " HTTP/1.1\r\nHost: ",Host, Headers, "\r\n\r\n"],
	send(Socket, list_to_binary(Request)),
	pull_response(Socket),
	gen_tcp:close(Socket).

send(Socket,<<Chunk:1024/binary, Rest/binary>>) ->
	gen_tcp:send(Socket, Chunk),
	send(Socket, Rest);
send(Socket, Rest) -> gen_tcp:send(Socket, Rest).

pull_response(Socket) ->
    case gen_tcp:recv(Socket, 0, 10000) of
		{ok, Binary} -> 
			io:format("Pull response from pid ~p, with body: ~n ~p ~n",[self(), binary_to_list(Binary)]),
			pull_response(Socket);
		{error, closed} -> io:format("Close request for pid ~p ~n",[self()]);
		{error,timeout} -> 
			io:format("Pull Request timeout for pid:~p ~n",[self()])
	end.
