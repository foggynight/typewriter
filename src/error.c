// SPDX-License-Identifier: GPL-2.0
// Copyright (C) 2020 Robert Coffey

#include <stdio.h>
#include <stdlib.h>

#include "error.h"

void fatal_error(char *str, int code)
{
    fprintf(stderr, "led: %s\n", str);
    exit(code);
}
