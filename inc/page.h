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
} page_t;

page_t *page_init(void);
int page_fopen(page_t *page, char *path);

#endif // PAGE_H_
