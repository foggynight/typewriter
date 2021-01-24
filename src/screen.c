/**
 * screen.c
 * Copyright (C) 2020-2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include <locale.h>
#include <ncurses.h>

#include "screen.h"
#include "rtb/log.h"

void screen_init(void)
{
    setlocale(LC_ALL, "");
    initscr();
    cbreak();
    noecho();
    intrflush(stdscr, FALSE);
    keypad(stdscr, TRUE);
}

void screen_kill(void)
{
    endwin();
}
