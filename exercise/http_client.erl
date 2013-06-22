-module(http_client).
-compile(export_all).

-define(OPTION,[{packet, 0}, {active, false}]).
-define(ACTIVE_OPTION,[{packet, 0}, {active, true}]).
-define(TIMEOUT,30000).
-define(HTTP_TERMINAL, "\r\n\r\n").

erl() ->
	http_get("www.erlang.org", "/"). % extract content-legnth and body manully.

erl2() ->
	http_get2("www.erlang.org", "/").  % do not handler extraction.

http_get(Host, Path) ->
	spawn(fun() -> doget(Host, Path) end).

http_get2(Host, Path) ->
	spawn(fun() -> doget2(Host, Path) end).

doget(Host, Path) -> 
	{ok, Socket} = gen_tcp:connect(Host, 80, ?OPTION),
	Headers = "",
	Request = ["GET ", Path, " HTTP/1.1\r\nHost: ",Host, Headers, ?HTTP_TERMINAL],
	send(Socket, list_to_binary(Request)),
	pull_response(Socket, [], 1),
	io:format("----Closing socket manually"),
	gen_tcp:close(Socket).

doget2(Host, Path) -> 
	{ok, Socket} = gen_tcp:connect(Host, 80, ?ACTIVE_OPTION),
	Headers = "Connection: Close",
	Request = ["GET ", Path, " HTTP/1.1\r\nHost: ",Host, Headers, ?HTTP_TERMINAL],
	send(Socket, list_to_binary(Request)),
	receive_response(Socket, [], 1),
	io:format("----Closing socket manually"),
 	gen_tcp:close(Socket).

receive_response(Socket, BinaryList, Count) ->
	receive 
		{tcp,Socket, Binary} -> 
			log_now(),
			io:format("Receive_response from pid ~p ,count ~p~n",[self(),Count]),
			receive_response(Socket, [Binary | BinaryList], Count + 1);
		{tcp_closed, Socket} -> 
	 		io:format(">>>>>>>Finish pull response from pid ~p, with body: ~n ~p ~n",[self(), lists:reverse(BinaryList)]),
			log_now()
	end.

check_header(Data) ->
	case re:split(Data, ?HTTP_TERMINAL, [{return,list},trim]) of
		[Data] -> false;
		[Header | Tail] -> {true, Tail, get_content_length(Header) - string:len(Tail)}
	end.
	
get_content_length(Header) ->
	case re:run(Header,"Content-Length: (\\d+)(\\r\\n)?",[global,{capture,[1],list}]) of
		nomatch -> 
			io:format("Extract Content-legth faild on Header:~p ~n", [Header]),
			0;
		{match,[[Len]]} -> list_to_integer(Len)
	end.

pull_response(Socket, BinaryList, Count) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
		{ok, Binary} -> 
			log_now(),
			io:format("Pull response from pid ~p ,count ~p~n",[self(),Count]),
			BinarySoFar = [Binary | BinaryList],
			case check_header(lists:reverse(BinarySoFar)) of
				{true, Body, Bodylength} -> 
					pull_body(Socket, Bodylength, Body);
				_ ->
					pull_response(Socket, BinarySoFar, Count + 1)
			end;
		{error, closed} -> 
			io:format(">>>>>>>Finish pull response from pid ~p, with body: ~n ~p ~n",[self(), lists:reverse(BinaryList)]),
			log_now();
		{error,timeout} -> 
			io:format("*******Pull Resonpse timeout for pid:~p with body: ~n ~p ~n",[self(), lists:reverse(BinaryList)]),
			log_now()
	end.

pull_body(Socket, Length, BinaryList) ->
	io:format("Going to pull body with content-length: ~p ~n",[Length]),
    case gen_tcp:recv(Socket, Length, ?TIMEOUT) of
		{ok, Data} -> 
			io:format("Pull body from pid ~p with body ~n ~p ~n",[self(), lists:reverse([Data | BinaryList])]);
		{error, closed} -> 
			io:format(">>>>>>>Close pull body for pid ~p ~n",[self()]);
		{error,timeout} -> 
			io:format("*******Pull Body timeout for pid:~p ~n",[self()])
	end.

log_now() ->
	{H,M,S} = time(),
	io:format("Time: ~p:~p:~p~n",[H,M,S]).

send(Socket,<<Chunk:1024/binary, Rest/binary>>) ->
	gen_tcp:send(Socket, Chunk),
	send(Socket, Rest);
send(Socket, Rest) -> gen_tcp:send(Socket, Rest).