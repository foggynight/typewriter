#!/usr/bin/env bash

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

g++ -g -I./inc src/*.cpp -o led_debug -lncurses
gdb led_debug
rm -f led_debug
