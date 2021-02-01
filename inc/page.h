/**
 * led - page.h
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef PAGE_H_
#define PAGE_H_

#include <stdio.h>

/* page_t: Page type representing a text file */
typedef struct page {
    /* File data */
    char *path; // Path to the file represented by the page
    FILE *file; // File pointer to the file represented by the page

    /* Content data */
    int length;   // Number of lines on the page
    char **lines; // Line buffer containing page text content
} page_t;

/**
 * page_init: Allocate and initialize a page
 * @return New page with a pre-allocated line buffer
 **/
page_t *page_init(void);

/**
 * page_load: Load file content onto a page
 *
 * @param page Target page to fill with text
 * @param path Path to the file to load from
 *
 * @return Number of lines in the file or -1 if the file doesn't exist
 **/
int page_load(page_t *page, const char *path);

#endif // PAGE_H_
