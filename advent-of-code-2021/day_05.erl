-module(day_05).
-export([main/1]).
-mode(compile).

parse_line(Line) ->
    [P1B, P2B] = binary:split(Line, <<" -> ">>, []),
    [X1B, Y1B] = binary:split(P1B, <<",">>, []),
    [X2B, Y2B] = binary:split(P2B, <<",">>, []),
    {
        binary_to_integer(X1B),
        binary_to_integer(Y1B),
        binary_to_integer(X2B),
        binary_to_integer(Y2B)
    }.

-define(COORDS(X, Y), (X bsl 10) bor Y).

set_point(X, Y, {_, More} = Map) when is_map_key(?COORDS(X, Y), More) ->
    Map;
set_point(X, Y, {One, More}) when is_map_key(?COORDS(X, Y), One) ->
    {One, sets:add_element(?COORDS(X, Y), More)};
set_point(X, Y, {One, More}) ->
    {sets:add_element(?COORDS(X, Y), One), More}.

intersections2(Lines) ->
    lists:foldl(
        fun({X1, Y1, X2, Y2}, Map) ->
            case {X2 - X1, Y2 - Y1} of
                {0, 0} ->
                    set_point(X1, Y1, Map);
                {0, N} when N > 0 ->
                    ekk:range_fold(fun(Y, M) -> set_point(X1, Y, M) end, Map, Y1, Y2);
                {0, N} when N < 0 ->
                    ekk:range_fold(fun(Y, M) -> set_point(X1, Y, M) end, Map, Y2, Y1);
                {N, 0} when N > 0 ->
                    ekk:range_fold(fun(X, M) -> set_point(X, Y1, M) end, Map, X1, X2);
                {N, 0} when N < 0 ->
                    ekk:range_fold(fun(X, M) -> set_point(X, Y1, M) end, Map, X2, X1);
                {N, N2} when (N > 0) and (N2 > 0) ->
                    ekk:range_fold(fun(X, M) -> set_point(X1 + X, Y1 + X, M) end, Map, 0, N);
                {N, N2} when (N > 0) and (N2 < 0) ->
                    ekk:range_fold(fun(X, M) -> set_point(X1 + X, Y1 - X, M) end, Map, 0, N);
                {N, N2} when (N < 0) and (N2 < 0) ->
                    ekk:range_fold(fun(X, M) -> set_point(X1 - X, Y1 - X, M) end, Map, 0, -N);
                {N, N2} when (N < 0) and (N2 > 0) ->
                    ekk:range_fold(fun(X, M) -> set_point(X1 - X, Y1 + X, M) end, Map, 0, -N)
            end
        end,
        {sets:new([{version, 2}]), sets:new([{version, 2}])},
        Lines
    ).

count_intersections({_, More}) ->
    sets:size(More).

intersections1(L1) ->
    Lines = lists:filter(fun({X1, Y1, X2, Y2}) -> (X1 == X2) or (Y1 == Y2) end, L1),
    intersections2(Lines).

main([File]) ->
    {ok, Input} = ekk:read_lines(File, fun parse_line/1),
    I = count_intersections(intersections1(Input)),
    io:format("Part One: ~w\n", [I]),
    I2 = count_intersections(intersections2(Input)),
    io:format("Part Two: ~w\n", [I2]).
