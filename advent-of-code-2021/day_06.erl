-module(day_06).
-export([main/1]).
-mode(compile).

% read_nums(<<N:1/binary>>, Acc) ->
%     [binary_to_integer(N) | Acc];

% read_nums(<<N:1/binary,$,,Rest/binary>>, Acc) ->
%     read_nums(Rest, [binary_to_integer(N) | Acc]).

% read_nums(Binary) ->
%     read_nums(Binary, []).

% simulate([], N2, 0) ->
%     N2;

% simulate([], N2, ToAdd) ->
%     simulate([], [8 | N2], ToAdd - 1);

% simulate([0 | Rest], N2, ToAdd) ->
%     simulate(Rest, [6 | N2], ToAdd + 1);

% simulate([N | Rest], N2, ToAdd) ->
%     simulate(Rest, [N - 1 | N2], ToAdd).

% simulate_times(N, 0) ->
%     length(N);

% simulate_times(N, Times) ->
%     N2 = simulate(N, [], 0),
%     simulate_times(N2, Times - 1).

read_nums(<<N:1/binary>>, Acc) ->
    I = binary_to_integer(N),
    array:set(I, array:get(I, Acc) + 1, Acc);
read_nums(<<N:1/binary, $,, Rest/binary>>, Acc) ->
    I = binary_to_integer(N),
    read_nums(Rest, array:set(I, array:get(I, Acc) + 1, Acc)).

read_nums(Binary) ->
    read_nums(Binary, array:new(9, {default, 0})).

simulate_step(State) ->
    lists:foldl(
        fun(N, A) ->
            case N of
                0 ->
                    array:set(8, array:get(0, State), A);
                7 ->
                    array:set(6, array:get(0, State) + array:get(7, State), A);
                _ ->
                    array:set(N - 1, array:get(N, State), A)
            end
        end,
        array:new(9, {default, 0}),
        lists:seq(0, 8)
    ).

simulate_times(A, 0) ->
    array:foldl(fun(_, X, Acc) -> X + Acc end, 0, A);
simulate_times(A, N) ->
    simulate_times(simulate_step(A), N - 1).

main([File]) ->
    {ok, [Input]} = ekk:read_lines(File, fun read_nums/1),
    L = simulate_times(Input, 80),
    io:format("Part One: ~w\n", [L]),
    L2 = simulate_times(Input, 256),
    io:format("Part Two: ~w\n", [L2]).
