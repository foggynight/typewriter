/**
 * main.c
 * Copyright (C) 2020-2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include <signal.h>
#include <stdlib.h>

#include "page.h"
#include "rtb/log.h"
#include "screen.h"

static void finish(int signal);

int main(int argc, char **argv)
{
    signal(SIGINT, finish);
    page_t *page = page_init();

    if (argc > 2)
        rtb_elog("led: too many arguments");
    else if (argc > 1)
        page_load(page, argv[1]);

    screen_init();

    while (1)
        screen_step(page);
}

static void finish(int signal)
{
    screen_kill();
    exit(0);
}
