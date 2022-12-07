-module(day_11).
-export([main/1]).
-mode(compile).


read_line(Line) ->
  lists:map(fun (N) -> N - $0 end, binary_to_list(Line)).


make_cavern([F|_] = Input) ->
  Width = length(F),
  Height = length(Input),
  Array = array:fix(array:from_list(lists:append(Input))),
  {Array, Width, Height}.


index_coords(X, Y, {_, W, H})
  when (X >= W) or (Y >= H) or (X < 0) or (Y < 0) ->
  0;
index_coords(X, Y, {A, W, _}) ->
  array:get(Y * W + X, A).


index_to_coords(I, {_, W, _}) ->
  X = I rem W,
  Y = I div W,
  {X, Y}.


count_flashes_around(I, Cavern) ->
  {X, Y} = index_to_coords(I, Cavern),
  lists:foldl(
    fun ({X1, Y1}, Sum) ->
      case index_coords(X1, Y1, Cavern) of
        flashing -> Sum + 1;
        _ -> Sum
      end
    end,
    0,
    [
     {X - 1, Y - 1},
     {X, Y - 1},
     {X + 1, Y - 1},
     {X - 1, Y},
     {X + 1, Y},
     {X - 1, Y + 1},
     {X, Y + 1},
     {X + 1, Y + 1}
    ]
  ).


%flash_octopi({A, W, H} = Cavern) ->
%  {A1, Flashes} = array:foldl(
%    fun (I, Octopus, {A1, Flashes}) ->
%      case Octopus of
%        flashing ->
%          {array:set(I, flashed, A1), Flashes};
%        flashed ->
%          {A1, Flashes};
%        N when N > 9 ->
%          {array:set(I, flashing, A1), Flashes + 1};
%        N ->
%          FlashesAround = count_flashes_around(I, Cavern),
%          {array:set(I, N + FlashesAround, A1), Flashes}
%      end
%    end, 
%    {A, 0},
%    A
%  ),
%  {{A1, W, H}, Flashes}.


flash_octopi(A) ->
  {A1, Flashes} = array:foldl(
    fun (I, Octopus, {A1, Flashes}) ->
      if
        is_integer(Octopus) and (Octopus > 9) ->
          {array:set(I, flashing, A1), Flashes + 1};
        true ->
          {A1, Flashes}
      end
    end,
    {A, 0},
    A
  ),
  {A1, Flashes}.


react_to_flashes({A, _, _} = Cavern) ->
  array:map(
    fun (I, Octopus) ->
      case Octopus of
        flashing ->
          flashed;
        flashed ->
          flashed;
        N ->
          FlashesAround = count_flashes_around(I, Cavern),
          N + FlashesAround
      end
    end,
    A
  ).


flash_octopi_loop({A, W, H}, Flashes) ->
  {A1, NewFlashes} = flash_octopi(A),
  if 
    NewFlashes > 0 ->
      A2 = react_to_flashes({A1, W, H}),
      flash_octopi_loop({A2, W, H}, Flashes + NewFlashes);
    true ->
      {A1, Flashes + NewFlashes}
  end.

flash_octopi_loop(Cavern) ->
  flash_octopi_loop(Cavern, 0).


turn_off_flashed_octopi(A) ->
  array:map(
    fun
      (_, flashed) -> 0;
      (_, O) -> O
    end,
    A
  ).


step_cavern({A, W, H}) ->
  A1 = array:map(fun (_, O) -> O + 1 end, A),
  {A2, Flashes} = flash_octopi_loop({A1, W, H}),
  A3 = turn_off_flashed_octopi(A2),
  {{A3, W, H}, Flashes}.


step_cavern(_, Flashes, 0) ->
  Flashes;
step_cavern(Cavern, Flashes, N) ->
  {C1, NewFlashes} = step_cavern(Cavern),
  step_cavern(C1, Flashes + NewFlashes, N - 1).

step_cavern(Cavern, N) ->
  step_cavern(Cavern, 0, N).


step_until_simultaneous_flash({_, W, H} = Cavern, N) ->
  {C1, Flashes} = step_cavern(Cavern),
  if
    Flashes =:= (W * H) ->
      N;
    true ->
      step_until_simultaneous_flash(C1, N + 1)
  end.

step_until_simultaneous_flash(Cavern) ->
  step_until_simultaneous_flash(Cavern, 1).


main([File]) ->
  {ok, Input} = ekk:read_lines(File, fun read_line/1),
  Cavern = make_cavern(Input),
  Flashes = step_cavern(Cavern, 100),
  io:format("Part One: ~w\n", [Flashes]),
  SimultaneousFlash = step_until_simultaneous_flash(Cavern),
  io:format("Part Two: ~w\n", [SimultaneousFlash]).
