-module(all).
-export([main/1]).
-mode(compile).

day(N, Module) ->
    io:format("Day ~2..0w\n", [N]),
    {T, _} = timer:tc(Module, main, [[io_lib:format("day_~2..0w.input", [N])]]),
    io:format("~w~ss\n\n", [T, unicode:characters_to_binary("μ")]).

main(_) ->
    day(1, day_01),
    day(2, day_02),
    day(3, day_03),
    day(4, day_04),
    day(5, day_05),
    day(6, day_06),
    day(7, day_07),
    day(8, day_08),
    day(9, day_09),
    day(10, day_10),
    day(11, day_11),
    day(12, day_12),
    day(13, day_13),
    day(14, day_14),
    day(16, day_16).
