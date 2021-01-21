/**
 * page.c
 * Copyright (C) 2020-2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "page.h"
#include "rtb/log.h"

page_t *page_init(void)
{
    page_t *new_page = calloc(1, sizeof new_page);
    if (!new_page)
        rtb_elog("page_init: calloc failed");

    return new_page;
}

int page_fopen(page_t *page, char *path)
{
    if (!page)
        rtb_elog("page_fopen: page argument is NULL");
    if (!path)
        rtb_elog("page_fopen: path argument is NULL");

    page->path = strdup(path);
    page->file = fopen(path, "r");

    return page->file ? 1 : 0;
}
