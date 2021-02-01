/**
 * led - screen.c
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include <locale.h>
#include <ncurses.h>

#include "cursor.h"
#include "page.h"
#include "rtb/log.h"
#include "screen.h"

static int scr_width;
static int scr_height;

void screen_setup(void)
{
    setlocale(LC_ALL, ""); // must flush locale before ncurses init

    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    intrflush(stdscr, FALSE);

    getmaxyx(stdscr, scr_height, scr_width); // TODO: resizable window
}

void screen_draw(cursor_t *cursor, page_t *page)
{
    /* draw lines of text to the screen, position of the cursor is
     * always at the center of the screen and the text slides */
    for (int i = 0; i < page->length; ++i)
        mvaddstr(i + scr_height/2 - cursor->y_pos,
                 scr_width/2 + cursor->x_pos,
                 page->lines[i]);
    refresh();
}

void screen_kill(void)
{
    endwin();
}
