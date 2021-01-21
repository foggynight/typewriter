/**
 * main.c
 * Copyright (C) 2020-2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include "page.h"
#include "rtb/log.h"

int main(int argc, char **argv)
{
    page_t *page = page_init();

    if (argc > 2) {
        rtb_log("led: too many arguments");
        return 1;
    }
    else if (argc > 1) {
        if (page_fopen(page, argv[1], "r")) {
            // load file content into text buffer
        }
    }

    // update loop

    // write file if necessary

    return 0;
}
