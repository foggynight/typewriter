# Copyright (C) 2020-2021 Robert Coffey
# Released under the GPLv2 license

CC=gcc
CFLAGS=-Wall -Wextra -Wpedantic
INCS=-I./inc/ -I./dep/rtb/inc/
LIBS=-lncurses

HEDS=$(wildcard inc/*.h)
SRCS=$(wildcard src/*.c)
OBJS=$(SRCS:%.c=%.o)
PROG=led

all: $(PROG)
	$(CC) $(CFLAGS) $(INCS) $(OBJS) -o $(PROG) $(LIBS)

$(PROG): $(OBJS)
$(OBJS): $(HEDS)
$(OBJS): $(SRCS)

%.o: %.c
	$(CC) $(CFLAGS) $(INCS) $< -c -o $@ $(LIBS)

.PHONY: clean
clean:
	rm -f $(OBJS)

.PHONY: remove
remove:
	rm -f $(OBJS) $(PROG)
