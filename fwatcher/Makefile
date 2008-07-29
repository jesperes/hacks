# GNU makefile

CFLAGS=-g -Wall -Werror

all: fwatcher

%.o: %.c
	$(CC) $(CFLAGS) $< -c -o $@

fwatcher: fwatcher.o
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

clean:
	$(RM) *.obj *.o *.exe

