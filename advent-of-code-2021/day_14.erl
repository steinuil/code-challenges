-module(day_14).
-export([main/1]).
-mode(compile).


read_line(<<P1/integer,P2/integer," -> ",Between/integer>>) ->
	{rule, {P1,P2}, Between};
read_line(Line) ->
	binary_to_list(Line).


step([], _, Acc) ->
	lists:reverse(Acc);
step([A|[B|_]=Rest], Rules, Acc) when is_map_key({A,B}, Rules) ->
	Between = map_get({A,B}, Rules),
	step(Rest, Rules, [Between,A|Acc]);
step([A|Rest], Rules, Acc) ->
	step(Rest, Rules, [A|Acc]).


replace_times(Template, _, 0) ->
	Tally = lists:foldl(fun (C, Tally) ->
		maps:update_with(C, fun (N) -> N + 1 end, 1, Tally)
	end, #{}, Template),
	V = maps:values(Tally),
	lists:max(V) - lists:min(V);

replace_times(Template, Rules, N) ->
	T = step(Template, Rules, []),
	replace_times(T, Rules, N - 1).


main([File]) ->
	{ok, [Template|R]} = ekk:read_lines(File, fun read_line/1),
	Rules = lists:foldl(fun ({rule, Key, Value}, Rules) -> maps:put(Key, Value, Rules) end, #{}, R),
	P1 = replace_times(Template, Rules, 10),
	io:format("Part One: ~w\n", [P1]),
	io:format("Part Two: ~w\n", [todo]).
