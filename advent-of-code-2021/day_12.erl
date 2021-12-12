-module(day_12).
-export([main/1]).
-mode(compile).


read_line(Line) ->
  case binary:split(Line, <<$->>) of
    [<<"start">>, A] ->
      {start, binary_to_list(A)};
    [A, <<"start">>] ->
      {start, binary_to_list(A)};
    [<<"end">>, A] ->
      {end_, binary_to_list(A)};
    [A, <<"end">>] ->
      {end_, binary_to_list(A)};
    [A, B] ->
      {binary_to_list(A), binary_to_list(B)}
  end.


add_to_graph(Key, Value, Graph) ->
  maps:update_with(Key, fun (Rest) -> sets:add_element(Value, Rest) end,
                   sets:add_element(Value, sets:new([{version,2}])), Graph).


build_graph({end_, A}, Graph) ->
  add_to_graph(A, end_, Graph);
build_graph({start, A}, Graph) ->
  add_to_graph(start, A, Graph);
build_graph({A, B}, Graph) ->
  add_to_graph(A, B, add_to_graph(B, A, Graph)).


build_graph(Input) ->
  G = lists:foldl(fun build_graph/2, #{}, Input),
  maps:filter(fun (_, V) -> not sets:is_empty(V) end, G).


add_paths(From, Graph, Visited) ->
  Paths = maps:get(From, Graph),
  sets:fold(fun (Path, Acc) ->
    walk_paths(Path, Graph, Visited) + Acc
  end, 0, Paths).


walk_paths(end_, _, _) ->
  1;
walk_paths(start, Graph, Visited) ->
  add_paths(start, Graph, Visited);
walk_paths([L|_] = From, Graph, Visited) when L >= $A, L =< $Z ->
  add_paths(From, Graph, Visited);
walk_paths(From, _, Visited) when is_map_key(From, Visited) ->
  0;
walk_paths(From, Graph, Visited) ->
  V = sets:add_element(From, Visited),
  add_paths(From, Graph, V).


add_paths2(From, Graph, Visited) ->
  Paths = maps:get(From, Graph),
  sets:fold(fun (Path, Acc) ->
    walk_paths2(Path, Graph, Visited) + Acc
  end, 0, Paths).


walk_paths2(end_, _, _) ->
  1;
walk_paths2(start, Graph, Visited) ->
  add_paths2(start, Graph, Visited);
walk_paths2([L|_] = From, Graph, Visited) when L >= $A, L =< $Z ->
  add_paths2(From, Graph, Visited);
walk_paths2(From, _, {_, From}) ->
  0;
walk_paths2(From, Graph, {Visited, false}) when is_map_key(From, Visited) ->
  add_paths2(From, Graph, {Visited, From});
walk_paths2(From, _, {Visited, _}) when is_map_key(From, Visited) ->
  0;
walk_paths2(From, Graph, {Visited, Twice}) ->
  V = sets:add_element(From, Visited),
  add_paths2(From, Graph, {V, Twice}).


main([File]) ->
	{ok, Input} = ekk:read_lines(File, fun read_line/1),
  Graph = build_graph(Input),
  PathCount = walk_paths(start, Graph, #{}),
  io:format("Part One: ~p\n", [PathCount]),
  PathCount2 = walk_paths2(start, Graph, {#{}, false}),
  io:format("Part Two: ~p\n", [PathCount2]).
