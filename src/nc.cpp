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
    move(page.cursor.pos.y, page.cursor.pos.x);
    refresh();
}

bool nc::input(int *dest)
{
    *dest = getch();
    return *dest != ERR;
}

void nc::update(Page& page, int input)
{
    if (isprint(input))
        page.add_char(input);
    else if (input == '\n')
        page.add_newline();
    else if (input == KEY_BACKSPACE)
        page.move_cursor(-1, 0);
    else if (input == KEY_F(2))
        page.file_write();
}
