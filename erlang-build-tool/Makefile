MODS= findtree ebt ebt_server inotify
ERL= erl
ERLC= erlc

.SUFFIXES: .erl .beam 
%.beam: %.erl
	$(ERLC) -W $<

all: compile
compile: $(MODS:%=%.beam)

clean:
	$(RM) *.beam *~ *.dump

