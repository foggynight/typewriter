# Copyright (C) 2021 Robert Coffey
# Released under the MIT license

CC=g++
FLAGS=-Wall -Wextra -Wpedantic
INCS=-I./inc/
LIBS=-lncurses

HEDS=$(wildcard inc/*.hpp)
SRCS=$(wildcard src/*.cpp)
OBJS=$(SRCS:%.cpp=%.o)
PROG=led

all: $(PROG)
	$(CC) $(FLAGS) $(INCS) $(OBJS) -o $(PROG) $(LIBS)

$(PROG): $(OBJS)
$(OBJS): $(HEDS)
$(OBJS): $(SRCS)

%.o: %.cpp
	$(CC) $(FLAGS) $(INCS) $< -c -o $@ $(LIBS)

.PHONY: install
install:
	cp -i $(PROG) ~/.local/bin

.PHONY: clean
clean:
	rm -f $(OBJS)

.PHONY: remove
remove:
	rm -f $(OBJS) $(PROG)

.PHONY: uninstall
uninstall:
	rm -f $(OBJS) $(PROG)
	rm -i ~/.local/bin/$(PROG)
