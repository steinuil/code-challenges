-module(ekk).
-export([read_lines/2, read_lines/1, list_index/2, permutations/1, find_map/2, list_sum_by/2]).

read_lines(File, Mapper) ->
	case file:read_file(File) of
		{ok, Lines} ->
			{ok, [Mapper(Line) || Line <- binary:split(Lines, <<"\n">>, [trim_all, global])]};
		Error -> Error
	end.

read_lines(File) ->
	read_lines(File, fun (X) -> X end).


list_index(0, [Item | _]) ->
	Item;

list_index(I, [_ | Rest])  ->
	list_index(I - 1, Rest).


permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L -- [H])].


find_map(_, []) ->
	false;

find_map(Fn, [H|T]) ->
	case Fn(H) of
		false -> find_map(Fn, T);
		{value, _} = Found -> Found
	end.


list_sum_by(Fn, List) ->
	lists:foldl(fun (Item, Sum) -> Fn(Item) + Sum end, 0, List).
