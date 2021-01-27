#!/usr/bin/env bash

# debug.sh - Build and debug led
# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

gcc -g -I./inc -I./dep/rtb/inc src/*.c -o led_debug -lncurses
gdb led_debug
rm -f led_debug
