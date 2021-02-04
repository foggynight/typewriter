/**
 * led - Screen.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include "Screen.hpp"

#include <cstdlib>
#include <ncurses.h>

#include "Page.hpp"

void Screen::init()
{
    // TODO: resizable window

    initscr();

    cbreak();
    noecho();

    keypad(stdscr, TRUE);
    intrflush(stdscr, FALSE);

    getmaxyx(stdscr, height, width);
}

void Screen::kill()
{
    endwin();
    exit(0);
}

void Screen::draw(Page& page)
{
    for (int i=0; i<height; ++i)
        mvaddstr(i, 0, page.lines[i].c_str());
    refresh();
}
