/**
 * page.h
 * Copyright (C) 2020-2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef PAGE_H_
#define PAGE_H_

#include <stdio.h>

typedef struct page {
    char *path;
    FILE *file;
    int length;
    char **lines;
} page_t;

page_t *page_init(void);
int page_load(page_t *page, const char *path);

#endif // PAGE_H_
