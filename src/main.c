#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULTLINEWIDTH 80

struct {
	unsigned int line_width;         // Maximum line width
	unsigned int line_width_set : 1; // Has maximum line width been set
} configs;

int get_args(int argc, char **argv);

int main(int argc, char **argv)
{
	configs.line_width = DEFAULTLINEWIDTH;
	get_args(argc, argv);

	return 0;
}

// get_args: Process the command line arguments
int get_args(int argc, char **argv)
{
	for (char **arg_ptr = argv+1; argc > 1; --argc, ++arg_ptr) {
		// Line width: [--lw|--line-width]
		if (!strcmp(*arg_ptr, "--lw") || !strcmp(*arg_ptr, "--line-width")) {
			char *error_str = "";
			--argc, ++arg_ptr;
			int line_width = strtol(*arg_ptr, &error_str, 10);
			if (*error_str || line_width < 1 || line_width > UINT_MAX) {
				printf("%s: invalid line width\n", *argv);
				exit(1);
			}
			else if (configs.line_width_set) {
				printf("%s: invalid use: line width already set\n", *argv);
				exit(1);
			}
			else {
				configs.line_width = line_width;
				configs.line_width_set = 1;
			}
		}
	}

	return 0;
}
