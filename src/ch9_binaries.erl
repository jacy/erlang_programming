-module(ch9_binaries).
-compile(export_all).

%% A binary is a reference to a chunk of raw, untyped memory. 
%% 1. bit
%% The most basic unit of data on a computer is a bit. And a bit is simply a 1 or a 0. In human terms, a bit is either used or not used; there is no in-between. 
%% 2. byte -> Group of Bits
%% When 8 bits are grouped together, it is then known as a byte. And bytes are what computers use to represent various characters such as those you see on your keyboard
%% 3. binary -> Bse 2 mathematics
%% only two units are used and everything is based on the power of 2 for each of the 8 possible bits in a single byte
%% 4. ASCII
%% The American Standard Code for Information Interchange (ASCII /ˈæski/)

%% ====================================================================
%% BIFs functions
%% ====================================================================
bifs() ->
	Bin1 = term_to_binary({test,12,true,[1,2,3]}),
	myio:p(Bin1),
	
	Term1 = binary_to_term(Bin1),
	myio:p(Term1),
	
	Bin2 = term_to_binary({cat,dog}),
	
	Bin3 = list_to_binary([Bin1, Bin2]),
	myio:p(Bin3),
	
	_Term2 = binary_to_term(Bin3),
	{Bin4,Bin5} = split_binary(Bin3,25),
	
	_Term4 = binary_to_term(Bin5),
	is_binary(Bin4).
%% ====================================================================
%% Bit Syntax -> Expr:Size/Type
%% ====================================================================
%% We use the term bitstring to mean an arbitrary-length sequence of bits, and the term binary to mean a string 
%% whose length is divisible by eight so that it can be seen as a sequence of bytes.
%% You can construct Bins using the following bit syntax:
%% Bin = <<E1, E2, ...,En>>
%% You can pattern-match them like this:
%% <<E1, E2, ...,En>> = Bin
%% The real strength of binaries lies in the fact that each expression in Bin can be qualified by a size and/or type qualification:
%% Expr:Size/Type
%% Sizes -> The size specified is in bits. The default size of an integer is 8, and of a float is 64 (eight bytes).
%% Types -> The type is a list of type specifiers, separated by hyphens. Type specifiers can be any of the following:
%% 1. A type -> The valid types are integer, float, binary, byte, bits, and bitstring.
%% 2. A sign -> The valid signs are signed and unsigned (the default). In the signed case, the first bit determines the sign: 0 is positive, 1 is negative.
%% 3. An endian value
%% 	Endian values are CPU-dependent. Endian values can be big (the default),little, or native. In the big-endian case, the first byte is the least significant one;
%% 	in little endian, the first byte is the most significant. You will have to take endian values only if you are transferring binaries across different CPU architectures. 
%% 	If you want your endian value to be determined at runtime, use native.
%% 4. A unit specification, such as unit:Val
%% 	The number of bits used by the entry will be Val*N, where N is the size of the value.
%% 	The default unit for bits and bitstring is 1; for binary and bytes it is 8.
%% ============================Example===================================
%% <<5:4,5:4>> returns <<"U">>. The integer 5 represented in four bits is equivalent to 0101. In our binary, we put two of them together, 01010101, which is the
%% integer 85, denoting the ASCII value of U. Put 85 in a list, and you get back the string  notation containing the capital letter U.
%%  Writing <<"Hello">> is the same as writing <<$H,$e,$l,$l,$o>>, or its ASCII equivalent, <<72,101,108,108,111>>.

%% ====================================================================
%% Pattern-Matching Bits
%% ====================================================================
%% you can use size and type qualifications in pattern matching. When types are omitted, the default type is an integer
match() ->
%% 	The system will not understand the expression B=<<1>>, as it will read it as B =< <1>> , so it is important to always separate the symbol << from equality with a space.
	Bin = <<1,17, 42:16>>,     % Equals to <<1:8,17:8,42:16>>
	<<_000D:16,_E,_F/binary>> = Bin, % Equals to <<D:16,E:8,F:8>>
	
	Frame = <<1,3,0,0,1,0,0,0>>,
	<<_Type, Size, _Bin:Size/binary-unit:8, _/binary>> = Frame, % bind Size and use it in the following match
	
	<<_X:7/bitstring,_Y:1/bitstring>> = <<42:8>>. % It is possible to match bitstrings of any length
%% ====================================================================
%% Bitstring Comprehensions
%% ====================================================================
%% A bitstring comprehension is delimited by << ... || ... >>, and <= is used (instead of <-) for generators. 
%% Most importantly, all bitstring entities are indicated by being enclosed within <<...>>, for example:
%% << <<bnot(X):1>> || <<X:1>> <= <<42:6>> >>.
%% ====================================================================
%% Bitwise Operators
%% ====================================================================
%% band Bitwise and
%% bor Bitwise or
%% bxor Bitwise exclusive or
%% bnot Bitwise negation
%% bsl Bit shift left; the second argument gives the size of the shift
%% bsr Bit shift right; the second argument gives the size of the shift























