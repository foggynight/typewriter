#include <stdio.h>
#include <stdlib.h>

#include "error.h"

void fatal_error(char *str, int code)
{
    fprintf(stderr, "led: %s\n", str);
    exit(code);
}
