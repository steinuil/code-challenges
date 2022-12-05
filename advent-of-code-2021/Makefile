days = \
	day_01.beam \
	day_02.beam \
	day_03.beam \
	day_04.beam \
	day_05.beam \
	day_06.beam \
	day_07.beam \
	day_08.beam \
	day_09.beam \
	day_10.beam \
	day_12.beam \
	day_13.beam \
	day_14.beam \
	day_16.beam


all: ekk.beam $(days)
	escript $@.erl

%.beam: %.erl
	erlc $<
