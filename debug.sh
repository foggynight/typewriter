#!/usr/bin/env bash
gcc -g -I./inc -I./dep/rtb/inc src/*.c -o led_debug -lncurses
gdb led_debug
rm -f led_debug
