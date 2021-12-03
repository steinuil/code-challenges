-module(day_01).
-export([main/1]).
-mode(compile).


count_incr([A | [B | _] = Rest], Acc) when B > A ->
	count_incr(Rest, Acc + 1);
count_incr([_ | Rest], Acc) ->
	count_incr(Rest, Acc);
count_incr([], Acc) ->
	Acc.

count_incr(Rest) ->
	count_incr(Rest, 0).


count_incr3([A | [_, _, B | _] = Rest], Acc) when B > A ->
	count_incr3(Rest, Acc + 1);
count_incr3([_ | Rest], Acc) ->
	count_incr3(Rest, Acc);
count_incr3([], Acc) ->
	Acc.

count_incr3(Rest) ->
	count_incr3(Rest, 0).


main([File]) ->
	{ok, Input} = ekk:read_lines(File, fun binary_to_integer/1),
	Count = count_incr(Input),
	io:format("Part One: ~w\n", [Count]),
	Count2 = count_incr3(Input),
	io:format("Part Two: ~w\n", [Count2]).
