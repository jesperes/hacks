APR_PREFIX=$(HOME)/hacks/apr
APR_CONFIG=$(APR_PREFIX)/bin/apr-1-config

# -- values derived from apr-1-config
CC= $(shell $(APR_CONFIG) --cc)
CFLAGS= $(shell $(APR_CONFIG) --cflags --cppflags --includes) 
LIBS= $(shell $(APR_CONFIG) --libs)
LDFLAGS= $(shell $(APR_CONFIG) --link-ld)

# -- our own values --

CFLAGS += -Wall -Werror -std=c99

CFLAGS := $(filter-out -O2, $(CFLAGS))

OBJ= bamboo.o env.o node.o

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

bamboo: $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@ $(LIBS)

clean:
	$(RM) $(OBJ)

clobber: clean
	$(RM) bamboo
