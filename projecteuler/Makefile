CFLAGS_GENERIC= -g -O3 -Wall -Werror -I/usr/local/include

CC=gcc
CFLAGS=$(CFLAGS_GENERIC) -std=c99

CXX=g++
CXXFLAGS=$(CFLAGS_GENERIC)

LIBS=-lgmp
LDFLAGS=-L/usr/local/lib

ALL=$(patsubst %.c, bin/%, $(wildcard problem*.c))
ALL_CPP=$(patsubst %.cpp, bin/%, $(wildcard problem*.cpp))
BINDIR:=$(shell mkdir -p bin)

default: $(ALL) $(ALL_CPP)

$(ALL): bin/%: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LIBS) $(LDFLAGS)

$(ALL_CPP): bin/%: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LIBS) $(LDFLAGS)

clean:
	rm -rf *.exe *.o *~ bin

