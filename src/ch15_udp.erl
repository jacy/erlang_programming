-module(ch15_udp).
-export([demo/0]).

%% ====================================================================
%% UDP
%% ====================================================================
%% User Datagram Protocol (UDP) is a connectionless protocol. UDP provides little error recovery.

%% clients on other hosts send their UDP packets to a listener socket which forwards them to an Erlang process. At any one time, only one process
%% is allowed to receive packets from a particular socket. This process is called the controlling process.

%% ====================================================================
%% Open socket
%% ====================================================================
%% To open a socket, on both the client and the server side, you use the following function calls:
%% 1. gen_udp:open(Port)
%% 2. gen_udp:open(Port, OptionList)
%% The Port is an integer denoting the listening port number of the socket.  The OptionList contains configuration
%% options which allow you to override the default values. The most useful parameters include:
%% 1. list
%% 		Forwards all messages in the packet as a list of integers, regardless of how they are sent. It is the default value if no option is chosen.
%% 2. binary
%% 		Forwards all messages in the packet as a binary.
%% 3. {header, Size}
%% 		Can be used if packets are being received as binaries. It splits the message into a list of size Size, the header, and the message (a binary)
%% 4. {active, false}
%% 		Sets the socket to passive mode. Instead of being sent, messages from the socket have to be retrieved using the gen_udp:recv/2 and gen_udp:recv/3 calls.
%% 5. {active, once}
%% 		Will send the first message it receives to the socket, but subsequent messages have to be retrieved using the recv functions.
%% 6. {ip, ip_address()}
%% 		specifies which of the interfaces the socket should use.
%% The call to open returns either {ok, Socket} or {error, Reason}, where Socket is the  identifier for the socket opened and Reason is one of several POSIX error codes returned
%% as an atom. The most common errors you will come across are :
%% 1. eaddrinuse  -> the address is already in use, 
%% 2. eaddrnotavail -> using a port in a range your OS has reserved, and eacces if you don’t have permission to open the socket.
demo() ->
	{ok, _Socket} = gen_udp:open(1234, [binary,{header,2}]).
%% ====================================================================
%% Close socket
%% ====================================================================
%% The gen_udp:close(Socket) call closes the socket and frees the port number allocated to it. It returns the atom ok.


%% ====================================================================
%% Send message
%% ====================================================================
%% If you want to send messages, you use the following function:gen_udp:send(Socket, Address, Port, Packet)


%% ====================================================================
%% Passive mode
%% ====================================================================
%% When the socket is opened in passive mode, the connected process has to explicitly retrieve the packet from the socket using these function calls:
%% 1. gen_udp:recv(Socket, Length)
%% 2. gen_udp:recv(Socket, Length, Timeout)
%% Length is relevant only to the raw transmission mode in TCP, and so it is ignored in UDP. If a packet has been received within the timeout, 
%% {ok, {Ip, PortNo, Packet}} is returned. If the bytes are not received within Timeout milliseconds {error, timeout} will be returned. 
%% If the receiving process calls gen_udp:recv when not in passive mode, expect to see the {error, einval} error.



































