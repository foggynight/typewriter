#include <stdio.h>
#include <stdlib.h>

#include "error.h"

// fatal_error: Print error message and exit program with an error code
void fatal_error(char *str, int code)
{
	fprintf(stderr, "led: %s\n", str);
	exit(code);
}
