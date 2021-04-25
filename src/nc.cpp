// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include "nc.hpp"

#include <cstdlib>
#include <ncurses.h>

#include "Page.hpp"

#define TAB_WIDTH 4

static int screen_width, screen_height;
static int screen_center_x, screen_center_y;

void nc::init()
{
    // TODO: resizable window

    initscr();

    cbreak();
    noecho();

    keypad(stdscr, TRUE);
    intrflush(stdscr, FALSE);

    getmaxyx(stdscr, screen_height, screen_width);

    screen_center_x = screen_width / 2;
    screen_center_y = screen_height / 2;
}

void nc::kill()
{
    endwin();
    exit(0);
}

void nc::draw(Page& page)
{
    clear();

    for (size_t i = 0; i < page.lines.size(); ++i)
        mvaddstr(i, screen_center_x - page.cursor.x, page.lines[i].c_str());

    move(screen_center_y, screen_center_x);
    refresh();
}

bool nc::input(int *dest)
{
    *dest = getch();
    return *dest != ERR;
}

void nc::update(Page& page, int input)
{
    if (isprint(input)) {      // Place the input character on the page at the
        page.add_char(input);  // cursor pos and move the cursor to the right
    }
    else {
        switch (input) {
            // Move the cursor to the start of the next line
            case '\n': page.add_newline(); break;

            // Move the cursor TAB_WIDTH to the right
            case '\t': page.move_cursor(TAB_WIDTH - (page.cursor.x % TAB_WIDTH), 0); break;

            // Move the cursor to the left
            case KEY_BACKSPACE: page.move_cursor(-1, 0); break;

            // Arrow keys - Move the cursor
            case KEY_UP: page.move_cursor(0, -1); break;
            case KEY_DOWN: page.move_cursor(0, 1); break;
            case KEY_LEFT: page.move_cursor(-1, 0); break;
            case KEY_RIGHT: page.move_cursor(1, 0); break;

            // Save the current page to its associated file
            case KEY_F(2): page.file_write(); break;
        }
    }
}
