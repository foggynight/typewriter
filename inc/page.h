/**
 * page.h
 * Copyright (C) 2020-2021 Robert Coffey
 **/

#ifndef PAGE_H_
#define PAGE_H_

#include <stdio.h>

typedef struct page {
    FILE *file;
} page_t;

page_t *page_init(void);

#endif // PAGE_H_
