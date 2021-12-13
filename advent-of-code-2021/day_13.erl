-module(day_13).
-export([main/1]).
-mode(compile).


read_line(<<"fold along x=",N/binary>>) ->
	{fold_x, binary_to_integer(N)};
read_line(<<"fold along y=",N/binary>>) ->
	{fold_y, binary_to_integer(N)};
read_line(Coord) ->
	[X, Y] = binary:split(Coord, <<$,>>),
	{coord, binary_to_integer(X), binary_to_integer(Y)}.


split_input(Input) ->
	lists:splitwith(fun ({coord, _, _}) -> true; (_) -> false end, Input).


plot(Nx, Mx) ->
	M = Mx + 1,
	N = Nx + 1,
	(M - abs(N rem (2 * M) - M)) - 1.


-define(COORDS(X, Y), (X bsl 10) bor Y).


plot_points(Coords, FX, FY) ->
	lists:foldl(fun ({coord, X, Y}, Acc) ->
		Point = ?COORDS(plot(X, FX), plot(Y, FY)),
		sets:add_element(Point, Acc)
	end, #{}, Coords).


part1(Coords, Folds) ->
	{value, FX1} = ekk:find_map(fun ({fold_x, F}) -> {value, F}; (_) -> false end, Folds),
	{value, FY1} = ekk:find_map(fun ({fold_y, F}) -> {value, F}; (_) -> false end, Folds),
	{FX, FY} = case Folds of
		[{fold_x, _}|_] -> {FX1, FY1 * 2 + 1};
		[{fold_y, _}|_] -> {FX1 * 2 + 1, FY1}
	end,
	Points = plot_points(Coords, FX, FY),
	sets:size(Points).


part2(Coords, FF) ->
	Folds = lists:reverse(FF),
	{value, FX} = ekk:find_map(fun ({fold_x, F}) -> {value, F}; (_) -> false end, Folds),
	{value, FY} = ekk:find_map(fun ({fold_y, F}) -> {value, F}; (_) -> false end, Folds),
	Points = plot_points(Coords, FX, FY),
	lists:map(fun (Y) ->
		lists:map(fun (X) ->
			Char = if is_map_key(?COORDS(X, Y), Points) -> "#"; true -> " " end,
			io:format("~s", [Char])
		end, lists:seq(0, FX - 1)),
		io:format("\n")
	end, lists:seq(0, FY - 1)).


main([File]) ->
	{ok, Input} = ekk:read_lines(File, fun read_line/1),
	{Coords, Folds} = split_input(Input),
	P1 = part1(Coords, Folds),
	io:format("Part One: ~w\n", [P1]),
	io:format("Part Two:\n"),
	part2(Coords, Folds).
