BISON=bison -y
FLEX=flex
CC=gcc
CFLAGS=-g -O0 -Wall
LDFLAGS=
LIBS=-lfl

all: lisp-parser

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

parser.tab.c: parser.y
	$(BISON) -d $^ -o $@

parser.tab.h: parser.tab.c

lexer.c: lexer.l parser.tab.h
	$(FLEX) -o$@ $<

lisp-parser: parser.tab.o lexer.o parser.o
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@  $(LIBS) 

clean:
	$(RM) *.o parser.tab.c parser.tab.h lisp-parser
