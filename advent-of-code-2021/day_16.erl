-module(day_16).
-export([main/1]).
-mode(compile).


read_line(Line) ->
	<< begin X = list_to_integer([C0, C1], 16), <<X>> end || <<C0:8,C1:8>> <= Line >>.


parse_literal(<< 0:1, N:4, Rest/bits >>, Number) ->
	{(Number bsl 4) bor N, Rest};
parse_literal(<< 1:1, N:4, Rest/bits >>, Number) ->
	parse_literal(Rest, (Number bsl 4) bor N).

parse_literal(Bits) ->
	parse_literal(Bits, 0).


parse_packets_subbits(<< >>, Packets) ->
	lists:reverse(Packets);

parse_packets_subbits(Bin, Packets) ->
	{Packet, Rest} = parse_packet(Bin),
	parse_packets_subbits(Rest, [Packet|Packets]).

parse_packets_subbits(Bin) ->
	parse_packets_subbits(Bin, []).


parse_packets_counted(Rest, 0, Packets) ->
	{lists:reverse(Packets), Rest};

parse_packets_counted(Bin, N, Packets) ->
	{Packet, Rest} = parse_packet(Bin),
	parse_packets_counted(Rest, N - 1, [Packet|Packets]).

parse_packets_counted(Bin, N) ->
	parse_packets_counted(Bin, N, []).


operator(0) -> sum;
operator(1) -> product;
operator(2) -> minimum;
operator(3) -> maximum;
operator(5) -> greater_than;
operator(6) -> less_than;
operator(7) -> equal_to.


parse_packet(<< Version:3, 4:3, Rest/bits >>) ->
	{Number, R} = parse_literal(Rest),
	{{Version, literal, Number}, R};

parse_packet(<< Version:3, TypeID:3, 0:1, BitLength:15, Subpackets:BitLength/bits, Rest/bits >>) ->
	Packets = parse_packets_subbits(Subpackets),
	Op = operator(TypeID),
	{{Version, Op, Packets}, Rest};

parse_packet(<< Version:3, TypeID:3, 1:1, SubpacketCount:11, Rest/bits >>) ->
	{Packets, R} = parse_packets_counted(Rest, SubpacketCount),
	Op = operator(TypeID),
	{{Version, Op, Packets}, R}.


parse_transmission(Bin) ->
	{Packet, Rest} = parse_packet(Bin),
	% Assert that the padding is 0
	S = bit_size(Rest),
	<<0:S>> = Rest,
	Packet.


add_versions({Version, literal, _}) ->
	Version;

add_versions({Version, _, Packets}) ->
	Version + ekk:list_sum_by(fun add_versions/1, Packets).


execute({_, literal, N}) ->
	N;

execute({_, sum, Packets}) ->
	ekk:list_sum_by(fun execute/1, Packets);

execute({_, product, Packets}) ->
	lists:foldl(fun (Packet, Product) -> Product * execute(Packet) end, 1, Packets);

execute({_, minimum, Packets}) ->
	lists:min(lists:map(fun execute/1, Packets));

execute({_, maximum, Packets}) ->
	lists:max(lists:map(fun execute/1, Packets));

execute({_, greater_than, [P1, P2]}) ->
	N1 = execute(P1),
	N2 = execute(P2),
	if
		N1 > N2 -> 1;
		true -> 0
	end;

execute({_, less_than, [P1, P2]}) ->
	N1 = execute(P1),
	N2 = execute(P2),
	if
		N1 < N2 -> 1;
		true -> 0
	end;

execute({_, equal_to, [P1, P2]}) ->
	N1 = execute(P1),
	N2 = execute(P2),
	if 
		N1 == N2 -> 1;
		true -> 0
	end.


main([File]) ->
	{ok, [Input]} = ekk:read_lines(File, fun read_line/1),
	Packet = parse_transmission(Input),
	VersionSum = add_versions(Packet),
	io:format("Part One: ~w\n", [VersionSum]),
	Result = execute(Packet),
	io:format("Part Two: ~w\n", [Result]).
