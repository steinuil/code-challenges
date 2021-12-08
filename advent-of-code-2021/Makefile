days = \
	day_01.beam \
	day_02.beam \
	day_03.beam \
	day_04.beam \
	day_05.beam \
	day_06.beam \
	day_07.beam \
	day_08.beam


all: ekk.beam $(days)
	escript $@.erl

%.beam: %.erl
	erlc $<
