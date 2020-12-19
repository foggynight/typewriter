// Copyright (C) 2020 Robert Coffey
// Released under the GPLv2 license

#ifndef LINE_H
#define LINE_H

typedef unsigned int uint;

// Line: A line of text
typedef struct line {
    uint number;       // Line number
    char *text;        // Text content
    struct line *prev; // Previous line
    struct line *next; // Next line
} Line;

#endif
