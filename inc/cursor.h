/**
 * led - cursor.h
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef CURSOR_H_
#define CURSOR_H_

typedef struct cursor {
    int x_pos;
    int y_pos;
} cursor_t;

cursor_t *cursor_init(void);
cursor_t *cursor_destroy(cursor_t *target);

#endif // CURSOR_H_
