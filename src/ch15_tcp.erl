-module(ch15_tcp).
-compile(export_all).
-define(PORT, 1235).

%% ====================================================================
%% Transmission Control Protocol
%% ====================================================================
%% Unlike UDP, with TCP package reception is guaranteed and packages are received in the same order they are sent. Common uses
%% of TCP include HTTP requests, peer-to-peer applications, and IM client/server connections. Erlang distribution is built on top of TCP.

%% On an architectural level, the main difference between TCP and UDP is that once you’ve opened a socket connection using TCP, 
%% it is kept open until either side closes it or it terminates because of an error. When setting up a connection, you would often 
%% spawn a new process for every request, keeping it alive for as long as the request is being handled.

%% ====================================================================
%% Defining the accept process
%% ====================================================================
%% There are two mechanisms for defining the accept process:
%% 1. The first option is to spawn a new process which becomes the accept process, while the listener goes back and listens for a new connection request.
%% 2. The second option, is to make the listener process the accept process, and spawn a new process which becomes the new listener.

%% ====================================================================
%% Start Socket
%% ====================================================================
%% gen_tcp:listen(PortNumber, Options) This starts a listener socket, which then waits for incoming connections. The call takes
%% the same options as the call to gen_udp:open/2 described earlier, as well as the following TCP-specific ones:
%% 1. {active, true}
%% 	Ensures that all messages received from the socket are forwarded as Erlang messages to the process that owns the socket. 
%% 	This active mode is the default value when opening a socket.
%% 2. {active, false}
%% 	Sets the socket to passive mode. Messages received from the socket are buffered, and the process must retrieve them through 
%% 	the gen_tcp:recv/2 and gen_tcp:recv/3 calls.
%% 3. {active, once}
%% 	Will set the socket to active mode, but as soon as the first message is received, it sets it to passive mode so that subsequent 
%% 	messages have to be retrieved using the recv functions.
%% 4. {keepalive, true}
%% 	Ensures that the connected socket sends keepalive messages when no data is being transferred. As “close socket” messages can be lost, 
%% this option ensures that the socket is closed if no response to the keepalive is received. By default, the flag is turned off.
%% 5. {nodelay, true}
%% Will result in the socket immediately sending the package, no matter how small.By default, this option is turned off and data is instead 
%% aggregated and sent in larger chunks.
%% 6. {packet_size, Integer}
%% Sets the maximum allowed length of the body. If packets are larger than Size, the packet is considered invalid.

%% ====================================================================
%% Controlling process
%% ====================================================================
%% The controlling process is generally the process that established a connection through calling one of 
%% gen_tcp:accept or gen_tcp:connect. To redirect messages elsewhere and pass the control to another process, 
%% the controlling process has to call gen_tcp:controlling_process(Socket, Pid)

%% ====================================================================
%% TCP Example Client
%% ====================================================================
client(Host, Data) ->
	{ok, Socket} = gen_tcp:connect(Host, ?PORT, [binary,{packet, 0}]),
	send(Socket, Data),
%% 	You close the socket using the gen_tcp:close(Socket) call. This can be made on either the client or the server side. 
%% In either case, the {tcp_closed, Socket} message will be sent to the socket on the other side
	ok = gen_tcp:close(Socket).

send(Socket,<<Chunk:100/binary, Rest/binary>>) ->
	gen_tcp:send(Socket, Chunk),
	send(Socket, Rest);
send(Socket, Rest) -> gen_tcp:send(Socket, Rest).

%% ====================================================================
%% TCP Example Server
%% ====================================================================
%% When the request arrives, the listener process becomes the accept process and is ready to receive binaries in passive mode. 
%% A new listener process is spawned and waits for the next connection request. The accept process continues receiving data 
%% from the client, appending it to a list until the socket is closed, after which it saves the data to a file
server() ->
%% 	the gen_tcp:listen/2 call returns immediately
	{ok, ListenSocket} = gen_tcp:listen(?PORT, [binary,{active, false}]), % passive mode
	wait_connect(ListenSocket, 0).

wait_connect(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket), % suspend the process until a request to connect is made to that socket
	spawn(?MODULE, wait_connect,[ListenSocket, Count + 1]),
	get_request(Socket, [], Count).


get_request(Socket, BinaryList, Count) ->
	% recv(Socket, Length, Timeout), The optional Timeout parameter specifies a timeout in milliseconds. The default value is infinity.
	case gen_tcp:recv(Socket, 
					  0,  %  If Length = 0, all available bytes are returned.
					   5000) of % will be timeout in 5 seconds.
		{ok, Binary} -> get_request(Socket, [Binary | BinaryList], Count);
		{error, closed} -> handle(lists:reverse(BinaryList), Count)
	end.

handle(Binary, Count) ->
	{ok, Fd} = file:open("log_file_" ++ integer_to_list(Count), write),
	file:write(Fd, Binary),
	file:close(Fd).
