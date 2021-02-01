/**
 * led - screen.h
 * Copyright (C) 2020-2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef SCREEN_H_
#define SCREEN_H_

#include "page.h"

void screen_init(void);
void screen_step(page_t *page);
void screen_kill(void);

#endif // SCREEN_H_
