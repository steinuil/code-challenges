-module(ekk).
-export([read_lines/2, read_lines/1, list_index/2, list_find_index/2, permutations/1, find_map/2]).

read_lines(File, Mapper) ->
	case file:read_file(File) of
		{ok, Lines} ->
			{ok, [Mapper(Line) || Line <- binary:split(Lines, <<"\n">>, [trim_all, global])]};
		Error -> Error
	end.

read_lines(File) ->
	read_lines(File, fun (X) -> X end).


list_index([Item | _], 0) ->
	Item;

list_index([_ | Rest], I)  ->
	list_index(Rest, I - 1).


list_find_index([], _, _) ->
	false;
list_find_index([H|Rest], Item, I) ->
	if
		H == Item -> I;
		true -> list_find_index(Rest, Item, I + 1)
	end.

list_find_index(List, Item) ->
	list_find_index(List, Item, 0).


permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L -- [H])].


find_map(_, []) ->
	false;

find_map(Fn, [H|T]) ->
	case Fn(H) of
		false -> find_map(Fn, T);
		{value, _} = Found -> Found
	end.
