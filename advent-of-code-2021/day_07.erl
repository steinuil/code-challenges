-module(day_07).
-export([main/1]).
-mode(compile).


read_nums(Line) ->
    [binary_to_integer(N) || N <- binary:split(Line, <<$,>>, [trim_all, global])].


index([Item | _], 0) ->
    Item;

index([_ | Rest], I)  ->
    index(Rest, I - 1).


min_fuel(Nums) ->
    Mean = index(lists:sort(Nums), round(length(Nums) / 2)),
    lists:foldl(fun (N, Sum) -> Sum + abs(Mean - N) end, 0, Nums).


min_fuel2(Nums) ->
    lists:min(
        lists:map(fun (Position) ->
            lists:sum(
                lists:map(fun (N) ->
                    X = abs(Position - N) + 1,
                    round((X * (X - 1)) / 2)
                end, Nums))
        end, lists:seq(lists:min(Nums), lists:max(Nums)))).


main([File]) ->
    {ok, [Input]} = ekk:read_lines(File, fun read_nums/1),
    F1 = min_fuel(Input),
    io:format("Part One: ~w\n", [F1]),
    F2 = min_fuel2(Input),
    io:format("Part Two: ~w\n", [F2]).