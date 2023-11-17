-module(pqueue).
-export([new/1, push/3, pop/2]).
-mode(compile).

% A priority queue with priorities [0, Size - 1).
% Items of priority of 0 are popped earlier than items of priority 1.
% Items of the same priority are popped FIFO.

new(Size) ->
    array:new([{size, Size}, {fixed, true}, {default, []}]).

push(Value, Priority, Q) ->
    Vs = array:get(Priority, Q),
    array:set(
        Priority,
        Vs ++ [Value],
        Q
    ).

pop(Priority, Q) ->
    Size = array:size(Q),
    if
        Priority >= Size ->
            empty;
        true ->
            case array:get(Priority) of
                [V | Vs] ->
                    {V, array:set(Priority, Vs, Q)};
                [] ->
                    pop(Q, Priority + 1)
            end
    end.
