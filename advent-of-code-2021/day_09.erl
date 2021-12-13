-module(day_09).
-export([main/1]).
-mode(compile).


read_line(Line) ->
	array:fix(array:from_list(binary_to_list(Line))).


get_xy(A, X, Y) ->
	try
		array:get(X, array:get(Y, A)) - $0
	catch _:_ ->
		9
	end.


is_low_point(Point, Around) ->
	case lists:all(fun (P) -> P > Point end, Around) of
		true -> Point + 1;
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


-define(COORDS(X, Y), (X bsl 10) bor Y).


flood_fill(_, X, Y, Filled) when is_map_key(?COORDS(X, Y), Filled) ->
  Filled;
flood_fill(Input, X, Y, Filled) ->
  case get_xy(Input, X, Y) >= 9 of
    true ->
      Filled;
    false ->
      F0 = sets:add_element(?COORDS(X, Y), Filled),
      F1 = flood_fill(Input, X + 1, Y, F0),
      F2 = flood_fill(Input, X - 1, Y, F1),
      F3 = flood_fill(Input, X, Y + 1, F2),
      F4 = flood_fill(Input, X, Y - 1, F3),
      F4
  end.


basins(Input, X, Y, MaxX, MaxY, Basins) ->
  B = case
    lists:all(fun (P) -> P > get_xy(Input, X, Y) end, [
      get_xy(Input, X + 1, Y),
      get_xy(Input, X - 1, Y),
      get_xy(Input, X, Y + 1),
      get_xy(Input, X, Y - 1)
    ])
  of
    false -> Basins;
    true -> [sets:size(flood_fill(Input, X, Y, #{}))|Basins]
  end,
	case {X, Y} of
		{MaxX, MaxY} ->
			B;
		{MaxX, _} ->
			basins(Input, 0, Y + 1, MaxX, MaxY, B);
		_ ->
			basins(Input, X + 1, Y, MaxX, MaxY, B)
	end.


part2(Input, MaxX, MaxY) ->
  Basins = basins(Input, 0, 0, MaxX, MaxY, []),
  [B1, B2, B3 | _] = lists:reverse(lists:sort(Basins)),
  B1 * B2 * B3.


main([File]) ->
	{ok, [First | _ ] = I} = ekk:read_lines(File),
	MaxY = length(I) - 1,
	MaxX = byte_size(First) - 1,
	Input = array:fix(array:from_list(lists:map(fun read_line/1, I))),
	P1 = low_points(Input, 0, 0, MaxX, MaxY, 0),
	io:format("Part One: ~w\n", [P1]),
  P2 = part2(Input, MaxX, MaxY),
  io:format("Part Two: ~w\n", [P2]).
