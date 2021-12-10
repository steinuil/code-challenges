-module(day_19).
-export([main/1]).
-mode(compile).


check_syntax([], Stack) ->
  Stack;
check_syntax([$)|Rest],[$(|Stack]) ->
  check_syntax(Rest, Stack);
check_syntax([$]|Rest],[$[|Stack]) ->
  check_syntax(Rest, Stack);
check_syntax([$}|Rest],[${|Stack]) ->
  check_syntax(Rest, Stack);
check_syntax([$>|Rest],[$<|Stack]) ->
  check_syntax(Rest, Stack);
check_syntax([$)|_],_) ->
  3;
check_syntax([$]|_],_) ->
  57;
check_syntax([$}|_],_) ->
  1197;
check_syntax([$>|_],_) ->
  25137;
check_syntax([Opening|Rest],Stack) ->
  check_syntax(Rest, [Opening|Stack]).

syntax_error_score(Line) ->
  case check_syntax(Line, []) of
    [_|_] -> 0;
    N -> N
  end.


completion_score([],Score) ->
  Score;
completion_score([$(|Rest],Score) ->
  completion_score(Rest, Score * 5 + 1);
completion_score([$[|Rest],Score) ->
  completion_score(Rest, Score * 5 + 2);
completion_score([${|Rest],Score) ->
  completion_score(Rest, Score * 5 + 3);
completion_score([$<|Rest],Score) ->
  completion_score(Rest, Score * 5 + 4).


completion_score(Line) ->
  case check_syntax(Line, []) of
    [_|_] = Stack ->
      completion_score(Stack, 0);
    _ -> 0
  end.


main([File]) ->
	{ok, Input} = ekk:read_lines(File, fun binary_to_list/1),
  Score = ekk:list_sum_by(fun syntax_error_score/1, Input),
  io:format("Part One: ~w\n", [Score]),
  CompletionScores = lists:filtermap(fun (Line) ->
    case completion_score(Line) of
      0 -> false;
      N -> {true, N}
    end
  end, Input),
  CompletionScore = ekk:list_index(floor(length(CompletionScores) / 2),
                                   lists:sort(CompletionScores)),
  io:format("Part Two: ~w\n", [CompletionScore]).
