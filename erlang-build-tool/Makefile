MODS= $(wildcard *.erl)
ERL= erl
ERLC= erlc -W -v +debug_info

CFLAGS=-g -Wall -Werror

.SUFFIXES: .erl .beam 
%.beam: %.erl
	$(ERLC) $<

all: compile
compile: $(MODS:%.erl=%.beam) inotify

%.o: %.c
	$(CC) $(CFLAGS) $< -c -o $@

inotify: inotify.o
	$(CC) $(CFLAGS) $< -o $@

clean:
	$(RM) *.beam *~ *.dump

