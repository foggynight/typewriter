/**
 * led - screen.h
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef SCREEN_H_
#define SCREEN_H_

#include "cursor.h"
#include "page.h"

void screen_setup(void);
void screen_draw(cursor_t *cursor, page_t *page);
void screen_kill(void);

#endif // SCREEN_H_