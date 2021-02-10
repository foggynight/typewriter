/**
 * led - nc.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include "nc.hpp"

#include <cstdlib>
#include <ncurses.h>

#include "Page.hpp"

void nc::init()
{
    // TODO: resizable window

    initscr();

    cbreak();
    noecho();

    keypad(stdscr, TRUE);
    intrflush(stdscr, FALSE);
}

void nc::kill()
{
    endwin();
    exit(0);
}

void nc::draw(Page& page)
{
    for (size_t i=0; i<page.lines.size(); ++i)
        mvaddstr(i, 0, page.lines[i].c_str());
    refresh();
}
