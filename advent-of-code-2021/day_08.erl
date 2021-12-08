-module(day_08).
-export([main/1]).
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


find_digit(String, Positions) ->
	N = lists:foldl(fun (Pos, Acc) ->
		I = maps:get(Pos, Positions),
		(1 bsl (6 - I)) bor Acc
	end, 0, String),
	maps:get(N, #{
		2#1110111 => 0,
		2#0010010 => 1,
		2#1011101 => 2,
		2#1011011 => 3,
		2#0111010 => 4,
		2#1101011 => 5,
		2#1101111 => 6,
		2#1010010 => 7,
		2#1111111 => 8,
		2#1111011 => 9
	}, false).


solve_positions(P) ->
	% Segments 'c' and 'f' are the segments of 1
	[[N1A, N1B] = N1, N7, N4, U5_1, U5_2, U5_3, U6_1, U6_2, U6_3, N8] =
		lists:sort(fun (A, B) -> (length(A) - length(B)) < 0 end, P),
	% Segment 'a' is the difference between the segments of 7 and 1
	[P0] = N7 -- N1,
	% Segments 'b' and 'd' are the difference between the the segments of 4 and 1
	[N4A, N4B] = N4 -- N1,
	Unknown = [U5_1, U5_2, U5_3, U6_1, U6_2, U6_3],
	% Try all permutations of 'b' 'c' 'd' and 'f'
	{value, Positions} = ekk:find_map(fun ({P2, P5}) ->
		ekk:find_map(fun ({P1, P3}) ->
			Known = [P0, P1, P2, P3, P5],
			% 9 is the only one of the numbers with 6 segments that contains
			% all the previously found segments
			case lists:search(fun (N) -> (Known -- N) =:= [] end, [U6_1, U6_2, U6_3]) of
				false -> false;
				{value, N9} ->
					% 'g' is the only segment of 9 that we don't know yet
					[P6] = N9 -- Known,
					% 'e' is the only segment of 8 that is not also in 9
					[P4] = N8 -- N9,
					Positions = #{P0 => 0, P1 => 1, P2 => 2, P3 => 3, P4 => 4, P5 => 5, P6 => 6},
					% If this solution is correct then all the remaining numbers
					% should map to a valid digit on the display.
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
