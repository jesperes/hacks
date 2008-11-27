CC=gcc
CFLAGS=-g -O3 -Wall -Werror -std=c99 -I/usr/local/include
LIBS=-lgmp
LDFLAGS=-L/usr/local/lib

ALL=$(patsubst %.c, bin/%, $(wildcard problem*.c))
BINDIR:=$(shell mkdir -p bin)

default: $(ALL)

$(ALL): bin/%: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LIBS) $(LDFLAGS)
clean:
	rm -rf *.exe *.o *~ bin

