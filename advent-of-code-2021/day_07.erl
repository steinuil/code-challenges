-module(day_07).
-export([main/1]).
-mode(compile).

read_nums(Line) ->
    [binary_to_integer(N) || N <- binary:split(Line, <<$,>>, [trim_all, global])].

min_fuel(Nums) ->
    Median = ekk:list_index(round(length(Nums) / 2), lists:sort(Nums)),
    lists:foldl(fun(N, Sum) -> Sum + abs(Median - N) end, 0, Nums).

min_fuel2(Nums) ->
    ekk:range_fold(
        fun(Position, Min) ->
            X = ekk:list_sum_by(
                fun(N) ->
                    X = abs(Position - N) + 1,
                    round((X * (X - 1)) / 2)
                end,
                Nums
            ),
            if
                X < Min -> X;
                true -> Min
            end
        end,
        0,
        lists:min(Nums),
        lists:max(Nums)
    ).

main([File]) ->
    {ok, [Input]} = ekk:read_lines(File, fun read_nums/1),
    F1 = min_fuel(Input),
    io:format("Part One: ~w\n", [F1]),
    F2 = min_fuel2(Input),
    io:format("Part Two: ~w\n", [F2]).
