MODS= $(wildcard *.erl)
ERL= erl
ERLC= erlc

.SUFFIXES: .erl .beam 
%.beam: %.erl
	$(ERLC) -W $<

all: compile
compile: $(MODS:%.erl=%.beam)

clean:
	$(RM) *.beam *~ *.dump

