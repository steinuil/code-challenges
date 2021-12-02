-module(day_02).
-export([main/1]).


parse_line(<<"forward ",N/integer>>) ->
	{forward, N - 48};
parse_line(<<"up ",N/integer>>) ->
	{up, N - 48};
parse_line(<<"down ",N/integer>>) ->
	{down, N - 48}.


calculate_position({forward, N}, {X, Y}) ->
	{X + N, Y};
calculate_position({up, N}, {X, Y}) ->
	{X, Y - N};
calculate_position({down, N}, {X, Y}) ->
	{X, Y + N}.


calculate_position2({down, N}, {X, Y, Aim}) ->
	{X, Y, Aim + N};
calculate_position2({up, N}, {X, Y, Aim}) ->
	{X, Y, Aim - N};
calculate_position2({forward, N}, {X, Y, Aim}) ->
	{X + N, Y + (Aim * N), Aim}.


main([File]) ->
	{ok, Input} = ekk:read_lines(File, fun (L) -> parse_line(L) end),
	{X, Y} = lists:foldl(fun (Command, Pos) -> calculate_position(Command, Pos) end, {0, 0}, Input),
	io:format("Part One: ~w\n", [X * Y]),
	{X1, Y1, _} = lists:foldl(fun (Command, Pos) -> calculate_position2(Command, Pos) end, {0, 0, 0}, Input),
	io:format("Part Two: ~w\n", [X1 * Y1]).
