/**
 * led - screen.c
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include <locale.h>
#include <ncurses.h>

#include "page.h"
#include "rtb/log.h"
#include "screen.h"

void screen_setup(void)
{
    setlocale(LC_ALL, "");
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    intrflush(stdscr, FALSE);
}

void screen_step(page_t *page)
{
    for (int i = 0; i < page->length; ++i)
        mvaddstr(i, 0, page->lines[i]);
    refresh();
}

void screen_kill(void)
{
    endwin();
}
