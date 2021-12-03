-module(day_03).
-export([main/1]).
-mode(compile).


parse_binary(<<$0,Rest/binary>>, N) ->
	parse_binary(Rest, N bsl 1);
parse_binary(<<$1,Rest/binary>>, N) ->
	parse_binary(Rest, (N bsl 1) bor 1);
parse_binary(<<>>, N) ->
	N.

parse_binary(Bin) ->
	parse_binary(Bin, 0).


calculate_gamma_rate(_, 0, Rate) ->
	Rate;
calculate_gamma_rate(Input, Digits, Rate) ->
	{Z, O} = lists:foldl(fun (N, {Z, O}) ->
		if
			(N band (1 bsl (Digits - 1))) /= 0 ->
				{Z, O + 1};
			true ->
				{Z + 1, O}
		end
	end, {0, 0}, Input),
	D = if
		Z > O ->
			0;
		true ->
			1
	end,
	calculate_gamma_rate(Input, Digits - 1, (Rate bsl 1) bor D).


calculate_epsilon_rate(GammaRate, Digits) ->
	(bnot GammaRate) band ((1 bsl Digits) - 1).


calculate_power_consumption(Input, Digits) ->
	GammaRate = calculate_gamma_rate(Input, Digits, 0),
	EpsilonRate = calculate_epsilon_rate(GammaRate, Digits),
	GammaRate * EpsilonRate.


oxygen_generator_rating(Input, Digits, Pos) ->
	Mask = ((1 bsl Pos) - 1) bsl (Digits - Pos),
	GammaRate = calculate_gamma_rate(Input, Digits, 0),
	Filtered = lists:filter(fun (N) -> N band Mask == GammaRate band Mask end, Input),
	case Filtered of
		[N] -> N;
		F -> oxygen_generator_rating(F, Digits, Pos + 1)
	end.


co2_scrubber_rating(Input, Digits, Pos) ->
	Mask = 1 bsl (Digits - Pos - 1),
	GammaRate = calculate_gamma_rate(Input, Digits, 0),
	EpsilonRate = calculate_epsilon_rate(GammaRate, Digits),
	Filtered = lists:filter(fun (N) -> N band Mask == EpsilonRate band Mask end, Input),
	case Filtered of
		[N] -> N;
		F -> co2_scrubber_rating(F, Digits, Pos + 1)
	end.


calculate_life_support_rating(Input, Digits) ->
	OxygenGeneratorRating = oxygen_generator_rating(Input, Digits, 0),
	CO2ScrubberRating = co2_scrubber_rating(Input, Digits, 0),
	OxygenGeneratorRating * CO2ScrubberRating.


main([File]) ->
	{ok, Lines} = ekk:read_lines(File),
	[Line | _] = Lines,
	Digits = byte_size(Line),
	Input = [parse_binary(N) || N <- Lines],
	Consumption = calculate_power_consumption(Input, Digits),
	io:format("Part One: ~w\n", [Consumption]),
	LifeSupportRating = calculate_life_support_rating(Input, Digits),
	io:format("Part Two: ~w\n", [LifeSupportRating]).
