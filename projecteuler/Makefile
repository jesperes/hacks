CC=gcc
CFLAGS=-g -O3 -Wall -Werror -std=c99

ALL=$(patsubst %.c, bin/%, $(wildcard problem*.c))
BINDIR:=$(shell mkdir -p bin)

default: $(ALL)

$(ALL): bin/%: %.c
	$(CC) $(CFLAGS) -o $@ $<
clean:
	rm -rf *.exe *.o *~ bin

