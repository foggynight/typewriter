/**
 * led - nc.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the MIT license
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
    move(page.cursor.y, page.cursor.x);
    refresh();
}

bool nc::input(int *dest)
{
    *dest = getch();
    return *dest != ERR;
}

void nc::update(Page& page, int input)
{
    if (isprint(input))       // Place the input character on the page at the
        page.add_char(input); // cursor pos and move the cursor to the right
    else {
        switch (input) {
            // Move the cursor to the start of the next line
            case '\n': page.add_newline(); break;

            // Move the cursor to the left
            case KEY_BACKSPACE: page.move_cursor(-1, 0); break;

            // Save the current page to its associated file
            case KEY_F(2): page.file_write(); break;

            // Arrow keys - Move the cursor
            case KEY_UP: page.move_cursor(0, -1); break;
            case KEY_DOWN: page.move_cursor(0, 1); break;
            case KEY_LEFT: page.move_cursor(-1, 0); break;
            case KEY_RIGHT: page.move_cursor(1, 0); break;
        }
    }
}
