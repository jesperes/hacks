
MODS= $(wildcard *.erl)
ERL= erl
ERLC= erlc -W -v +debug_info

.SUFFIXES: .erl .beam 
%.beam: %.erl
	$(ERLC) $<

all: compile
compile: $(MODS:%.erl=%.beam)

run: compile
	$(ERL) -noinput -run mencoder rip -run init stop

clean:
	$(RM) *.beam *~ *.dump

