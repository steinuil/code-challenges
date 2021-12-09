-module(day_09).
-export([main/1]).
-mode(compile).


read_line(Line) ->
	array:fix(array:from_list(binary_to_list(Line))).


get_xy(A, X, Y) ->
	try
		array:get(X, array:get(Y, A))
	catch _:_ ->
		10
	end.


is_low_point(Point, Around) ->
	case lists:all(fun (P) -> P > Point end, Around) of
		true -> Point - $0 + 1;
		false -> 0
	end.


low_points(Input, X, Y, MaxX, MaxY, Acc)	->
	P = is_low_point(get_xy(Input, X, Y), [
		get_xy(Input, X + 1, Y),
		get_xy(Input, X - 1, Y),
		get_xy(Input, X, Y + 1),
		get_xy(Input, X, Y - 1)
	]),
	case {X, Y} of
		{MaxX, MaxY} ->
			Acc + P;
		{MaxX, _} ->
			low_points(Input, 0, Y + 1, MaxX, MaxY, Acc + P);
		_ ->
			low_points(Input, X + 1, Y, MaxX, MaxY, Acc + P)
	end.


main([File]) ->
	{ok, [First | _ ] = I} = ekk:read_lines(File),
	MaxY = length(I) - 1,
	MaxX = byte_size(First) - 1,
	Input = array:fix(array:from_list(lists:map(fun read_line/1, I))),
	P1 = low_points(Input, 0, 0, MaxX, MaxY, 0),
	io:format("Part One: ~w\n", [P1]).
