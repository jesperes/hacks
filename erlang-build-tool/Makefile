
ifneq ($(shell sh -c '$(CC) -dM -E - < /dev/null | grep __GNUC__'),)
  CC_IS_GCC=yes
endif

ifeq ($(CC_IS_GCC),yes)
  CFLAGS= -g -O2 -Wall -Werror
ifeq ($(NO_CYGWIN),yes)
  CFLAGS += -mno-cygwin
endif
endif

MODS= $(wildcard *.erl)
ERL= erl
ERLC= erlc -W -v +debug_info

.SUFFIXES: .erl .beam 
%.beam: %.erl
	$(ERLC) $<

all: compile
compile: $(MODS:%.erl=%.beam) # fmon_driver

%.o: %.c
	$(CC) $(CFLAGS) $< -c -o $@

fmon_driver: fmon_driver.o
	$(CC) $(CFLAGS) $(ALL_LDFLAGS) $< -o $@

clean:
	$(RM) *.beam *~ *.dump *.o *.exe

