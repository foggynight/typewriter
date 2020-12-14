// Copyright (C) 2020 Robert Coffey
// Licensed under the GNU GPLv2

#include <stdio.h>
#include <stdlib.h>

#include "error.h"

void fatal_error(char *str, int code)
{
    fprintf(stderr, "led: %s\n", str);
    exit(code);
}
