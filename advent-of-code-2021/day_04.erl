-module(day_04).
-export([main/1]).
-mode(compile).


parse_line(<<N1:2/binary," ",N2:2/binary," ",N3:2/binary," ",N4:2/binary," ",N5:2/binary>>) ->
	lists:map(fun (N) -> binary_to_integer(string:trim(N)) end, [N1, N2, N3, N4, N5]).


parse_square([], Squares) ->
	Squares;
parse_square([L1, L2, L3, L4, L5 | Rest], Squares) ->
	Square = lists:flatmap(fun parse_line/1, [L1, L2, L3, L4, L5]),
	parse_square(Rest, [Square | Squares]).


parse_drawn(Drawn) ->
	lists:map(fun binary_to_integer/1, string:split(Drawn, ",", all)).


index_square(_, [], _, O) ->
	O;
index_square(D, [N | Rest], I, O) when N == D ->
	index_square(D, Rest, I - 1, O bor (1 bsl (I - 1)));
index_square(D, [_ | Rest], I, O) ->
	index_square(D, Rest, I - 1, O).

index_square(D, S, Mask) ->
	index_square(D, S, 25, Mask).


is_winner(D) ->
	lists:any(fun (M) -> (D band M) == M end, [
		2#0000000000000000000011111,
		2#0000000000000001111100000,
		2#0000000000111110000000000,
		2#0000011111000000000000000,
		2#1111100000000000000000000,
		2#0000100001000010000100001,
		2#0001000010000100001000010,
		2#0010000100001000010000100,
		2#0100001000010000100001000,
		2#1000010000100001000010000
	]).


filter_drawn([], _, Sum) ->
	Sum;
filter_drawn([E | Rest], Mask, Sum) ->
	if
		(Mask band 1) == 0 ->
			filter_drawn(Rest, Mask bsr 1, E + Sum);
		true ->
			filter_drawn(Rest, Mask bsr 1, Sum)
	end.

filter_drawn(Board, Mask) ->
	filter_drawn(lists:reverse(Board), Mask, 0).


calculate_score(Board, Mask, LastDrawn) ->
	NotCalled = filter_drawn(Board, Mask),
	NotCalled * LastDrawn.


draw_winner(Boards, [Drawn | RestDrawn]) ->
	B2 = [{Nums, index_square(Drawn, Nums, Mask)} || {Nums, Mask} <- Boards],
	case lists:search(fun ({_, Mask}) -> is_winner(Mask) end, B2) of
		false ->
			draw_winner(B2, RestDrawn);
		{value, {Nums, Mask}} ->
			calculate_score(Nums, Mask, Drawn)
	end.


draw_last_winner([{Nums, Mask}], [Drawn | RestDrawn]) ->
	M = index_square(Drawn, Nums, Mask),
	case is_winner(M) of
		true ->
			calculate_score(Nums, M, Drawn);
		false ->
			draw_last_winner([{Nums, M}], RestDrawn)
	end;
draw_last_winner(Boards, [Drawn | RestDrawn]) ->
	B2 = [{Nums, index_square(Drawn, Nums, Mask)} || {Nums, Mask} <- Boards],
	draw_last_winner(lists:filter(fun ({_, Mask}) -> not is_winner(Mask) end, B2), RestDrawn).


main([File]) ->
	{ok, Input} = ekk:read_lines(File),
	[D | Rest] = Input,
	Drawn = parse_drawn(D),
	Boards = parse_square(Rest, []),
	WinnerScore = draw_winner([{Nums, 0} || Nums <- Boards], Drawn),
	io:format("Part One: ~w\n", [WinnerScore]),
	LastWinnerScore = draw_last_winner([{Nums, 0} || Nums <- Boards], Drawn),
	io:format("Part Two: ~w\n", [LastWinnerScore]).
