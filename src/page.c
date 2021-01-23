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

// TODO: Add variable length buffer for lines
#define MAX_LINE_COUNT  64
#define MAX_LINE_LENGTH 256

static int page_fopen(page_t *page, const char *path, const char *mode);

page_t *page_init(void)
{
    page_t *new_page = calloc(1, sizeof new_page);
    if (!new_page)
        rtb_elog("page_init: calloc failed");

    new_page->lines = calloc(MAX_LINE_COUNT, sizeof(char *));
    if (!new_page->lines)
        rtb_elog("page_init: calloc failed");

    for (int i = 0; i < MAX_LINE_COUNT; ++i) {
        new_page->lines[i] = calloc(MAX_LINE_LENGTH, 1);
        if (!new_page->lines[i])
            rtb_elog("page_init: calloc failed");
    }

    return new_page;
}

int page_load(page_t *page, const char *path)
{
    if (!page)
        rtb_elog("page_load: page argument is NULL");
    if (!path)
        rtb_elog("page_load: path argument is NULL");

    if (!page_fopen(page, path, "r"))
        return -1;

    int count = 0;
    while (count < MAX_LINE_COUNT
        && fgets(page->lines[count], MAX_LINE_LENGTH+1, page->file))
    {
        ++count;
    }
    page->length = count;

    return count;
}

static int page_fopen(page_t *page, const char *path, const char *mode)
{
    if (!page)
        rtb_elog("page_fopen: page argument is NULL");
    if (!path)
        rtb_elog("page_fopen: path argument is NULL");
    if (!path)
        rtb_elog("page_fopen: mode argument is NULL");

    if (page->path)
        free(page->path);
    if (page->file)
        fclose(page->file);

    page->path = strdup(path);
    page->file = fopen(path, mode);

    return page->file ? 1 : 0;
}
