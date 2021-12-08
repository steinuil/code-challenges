-module(day_08).
-export([main/1, digits/0]).
-mode(compile).


read_line(Line) ->
	[P, O] = binary:split(Line, <<" | ">>, [trim_all]),
	P2 = binary:split(P, <<" ">>, [trim_all, global]),
	Patterns = lists:map(fun (Bin) -> [N - 97 || <<N>> <= Bin] end, P2),
	O2 = binary:split(O, <<" ">>, [trim_all, global]),
	Output = lists:map(fun (Bin) -> [N - 97 || <<N>> <= Bin] end, O2),
	{Patterns, Output}.


part1(Input) ->
	ekk:list_sum_by(fun ({_, Output}) ->
		N = ekk:list_sum_by(fun (String) -> 
			S = length(String),
			if
				(S == 2) or (S == 4) or (S == 3) or (S == 7) -> 1;
				true -> 0
			end
		end, Output),
		N
	end, Input).


digits() -> [
	2#1110111,
	2#0010010,
	2#1011101,
	2#1011011,
	2#0111010,
	2#1101011,
	2#1101111,
	2#1010010,
	2#1111111,
	2#1111011
].


find_digit(String, Positions) ->
	N = lists:foldl(fun (Pos, Acc) ->
		I = ekk:list_find_index(Positions, Pos),
		(1 bsl (6 - I)) bor Acc
	end, 0, String),
	ekk:list_find_index(digits(), N).


solve_positions(P) ->
	[[N1A, N1B] = N1, N7, N4, NX1, NX2, NX3, NX4, NX5, NX6, N8] =
		lists:sort(fun (A, B) -> (length(A) - length(B)) < 0 end, P),
	[P0] = N7 -- N1,
	[N4A, N4B] = N4 -- N1,
	Unknown = [NX1, NX2, NX3, NX4, NX5, NX6],
	{value, Positions} = ekk:find_map(fun ({P2, P5}) ->
		ekk:find_map(fun ({P1, P3}) ->
			Known = lists:sort([P0, P1, P2, P3, P5]),
			case lists:search(fun (N) -> (Known -- N) =:= [] end, Unknown) of
				false -> false;
				{value, N9} ->
					[P6] = N9 -- Known,
					[P4] = N8 -- N9,
					Positions = [P0, P1, P2, P3, P4, P5, P6],
					case lists:all(fun (Pattern) -> find_digit(Pattern, Positions) /= false end, Unknown) of
						true -> {value, Positions};
						false -> false
					end
			end
		end, [{N4A, N4B}, {N4B, N4A}])
	end, [{N1A, N1B}, {N1B, N1A}]),
	Positions.


part2(Input) ->
	ekk:list_sum_by(fun ({P, Output}) ->
		Positions = solve_positions(P),
		lists:foldl(fun (String, Digits) ->
			N = find_digit(String, Positions),
			N + (Digits * 10)
		end, 0, Output)
	end, Input).


main([File]) ->
	{ok, Input} = ekk:read_lines(File, fun read_line/1),
	P1 = part1(Input),
	io:format("Part One: ~w\n", [P1]),
	P2 = part2(Input),
	io:format("Part Two: ~w\n", [P2]).
