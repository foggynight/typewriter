/**
 * led - cursor.c
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include <stdlib.h>

#include "cursor.h"
#include "rtb/log.h"

cursor_t *cursor_init(void)
{
    cursor_t *new_cursor = calloc(1, sizeof new_cursor);
    if (!new_cursor)
        rtb_elog("cursor_init: calloc failed");

    return new_cursor;
}

cursor_t *cursor_destroy(cursor_t *target)
{
    if (!target)
        rtb_elog("cursor_destroy: target is NULL");

    free(target);

    return NULL;
}
