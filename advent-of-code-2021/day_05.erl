-module(day_05).
-export([main/1]).
-mode(compile).


parse_line(Line) ->
	[P1B, P2B] = binary:split(Line, <<" -> ">>, []),
	[X1B, Y1B] = binary:split(P1B, <<",">>, []),
	[X2B, Y2B] = binary:split(P2B, <<",">>, []),
	P1 = {binary_to_integer(X1B), binary_to_integer(Y1B)},
	P2 = {binary_to_integer(X2B), binary_to_integer(Y2B)},
	{P1, P2}.


encode_coord(X, Y) ->
	(X bsl 10) bor Y.


set_point(X, Y, Map) ->
	maps:update_with(encode_coord(X, Y), fun (N) -> N + 1 end, 1, Map).


intersections2(Lines) ->
	lists:foldl(fun ({{X1, Y1}, {X2, Y2}}, Map) ->
		case {X2 - X1, Y2 - Y1} of
			{0, 0} ->
				set_point(X1, Y1, Map);
			{0, N} when N > 0 ->
				lists:foldl(fun (Y, M) -> set_point(X1, Y, M) end, Map, lists:seq(Y1, Y2));
			{0, N} when N < 0 ->
				lists:foldl(fun (Y, M) -> set_point(X1, Y, M) end, Map, lists:seq(Y2, Y1));
			{N, 0} when N > 0 ->
				lists:foldl(fun (X, M) -> set_point(X, Y1, M) end, Map, lists:seq(X1, X2));
			{N, 0} when N < 0 ->
				lists:foldl(fun (X, M) -> set_point(X, Y1, M) end, Map, lists:seq(X2, X1));

			{N, N2} when (N > 0) and (N2 > 0) ->
				lists:foldl(fun (X, M) -> set_point(X1 + X, Y1 + X, M) end, Map, lists:seq(0, N));
			{N, N2} when (N > 0) and (N2 < 0) ->
				lists:foldl(fun (X, M) -> set_point(X1 + X, Y1 - X, M) end, Map, lists:seq(0, N));
			{N, N2} when (N < 0) and (N2 < 0) ->
				lists:foldl(fun (X, M) -> set_point(X1 - X, Y1 - X, M) end, Map, lists:seq(0, -N));
			{N, N2} when (N < 0) and (N2 > 0) ->
				lists:foldl(fun (X, M) -> set_point(X1 - X, Y1 + X, M) end, Map, lists:seq(0, -N))
		end
	end, #{}, Lines).


count_intersections(Map) ->
	maps:fold(fun (_, V, C) ->
		if
			V > 1 -> C + 1;
			true -> C
		end
	end, 0, Map).


intersections1(L1) ->
	Lines = lists:filter(fun ({{X1, Y1}, {X2, Y2}}) -> (X1 == X2) or (Y1 == Y2) end, L1),
	intersections2(Lines).


main([File]) ->
	{ok, Input} = ekk:read_lines(File, fun parse_line/1),
	I = count_intersections(intersections1(Input)),
	io:format("Part One: ~w\n", [I]),
	I2 = count_intersections(intersections2(Input)),
	io:format("Part Two: ~w\n", [I2]).
