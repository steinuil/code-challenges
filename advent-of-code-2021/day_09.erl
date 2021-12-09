-module(day_09).
-export([main/1]).
-mode(compile).


read_line(Line) ->
	array:fix(array:from_list(binary_to_list(Line))).


is_low_point(Point, Around) ->
	case lists:all(fun (P) -> P > Point end, Around) of
		true ->
		  Point - $0 + 1;
		false -> 0
	end.


get_xy(A, X, Y) ->
	array:get(X, array:get(Y, A)).


low_points(_, x, _, _, _, Acc) ->
	Acc;
low_points(Input, X, Y, MaxX, MaxY, Acc)  ->
	%io:format("~w,~w = ~w\n", [X, Y, get_xy(Input, X, Y) - $0]),

	{P, NextX, NextY} = if
		(X == 0) and (Y == 0) ->
			Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X + 1, Y),
				get_xy(Input, X, Y + 1)
			]),
			{Point, X + 1, Y};
		(X == 0) and (Y == MaxY) ->
		  Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X + 1, Y),
				get_xy(Input, X, Y - 1)
			]),
			{Point, X + 1, Y};
		(X == MaxX) and (Y == 0) ->
		  Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X - 1, Y),
				get_xy(Input, X, Y + 1)
			]),
			{Point, 0, Y + 1};
		(X == MaxX) and (Y == MaxY) ->
		  Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X - 1, Y),
				get_xy(Input, X, Y - 1)
			]),
			{Point, x, Y};
		(X == 0) ->
		  Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X + 1, Y),
				get_xy(Input, X, Y + 1),
				get_xy(Input, X, Y - 1)
			]),
			{Point, X + 1, Y};
		(X == MaxX) ->
		  Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X - 1, Y),
				get_xy(Input, X, Y + 1),
				get_xy(Input, X, Y - 1)
			]),
			{Point, 0, Y + 1};
		(Y == 0) ->
		  Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X + 1, Y),
				get_xy(Input, X - 1, Y),
				get_xy(Input, X, Y + 1)
			]),
			{Point, X + 1, Y};
		(Y == MaxY) ->
		  Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X + 1, Y),
				get_xy(Input, X - 1, Y),
				get_xy(Input, X, Y - 1)
			]),
			{Point, X + 1, Y};
		true ->
		  Point = is_low_point(get_xy(Input, X, Y), [
				get_xy(Input, X + 1, Y),
				get_xy(Input, X - 1, Y),
				get_xy(Input, X, Y + 1),
				get_xy(Input, X, Y - 1)
			]),
			{Point, X + 1, Y}
	end,

	low_points(Input, NextX, NextY, MaxX, MaxY, Acc + P).


main([File]) ->
	{ok, [First | _ ] = I} = ekk:read_lines(File),
	MaxY = length(I) - 1,
	MaxX = byte_size(First) - 1,
	Input = array:fix(array:from_list(lists:map(fun read_line/1, I))),
	P1 = low_points(Input, 0, 0, MaxX, MaxY, 0),
	io:format("Part One: ~w\n", [P1]).
