/**
 * page.c
 * Copyright (C) 2020-2021 Robert Coffey
 **/

#include <stdlib.h>

#include "page.h"
#include "rtb/log.h"

page_t *page_init(void)
{
    page_t *new_page = calloc(1, sizeof new_page);
    if (!new_page)
        rtb_elog("page_init: calloc failed");

    return new_page;
}
