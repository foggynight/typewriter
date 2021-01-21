# Copyright (C) 2020-2021 Robert Coffey
# Released under the GPLv2 license

CC=gcc
CFLAGS=-Wall -Wextra -Wpedantic
INCS=-I./inc/ -I./dep/rtb/inc/

HEDS=$(wildcard inc/*.h)
SRCS=$(wildcard src/*.c)
OBJS=$(SRCS:%.c=%.o)
PROG=led

all: $(PROG)
	$(CC) $(CFLAGS) $(INCS) $(OBJS) -o $(PROG)

$(PROG): $(OBJS)
$(OBJS): $(HEDS)
$(OBJS): $(SRCS)

%.o: %.c
	$(CC) $(CFLAGS) $(INCS) $< -c -o $@

.PHONY: clean
clean:
	rm -f $(OBJS)

.PHONY: remove
remove:
	rm -f $(OBJS) $(PROG)
