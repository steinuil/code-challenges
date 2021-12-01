-module(ekk).
-export([read_lines/2, read_lines/1]).

read_lines(File, Mapper) ->
	case file:read_file(File) of
		{ok, Lines} ->
			{ok, [Mapper(Line) || Line <- binary:split(Lines, <<"\n">>, [trim_all, global])]};
		Error -> Error
	end.

read_lines(File) ->
	read_lines(File, fun (X) -> X end).
