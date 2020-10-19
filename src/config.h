#ifndef CONFIG_H
#define CONFIG_H

#include <stdio.h>

typedef unsigned int uint;

// Config: Program configuration
typedef struct config {
	char *program_name;        // Program name
	FILE *input_stream;        // Program input stream
	uint input_stream_set : 1; // Has input stream been set
	FILE *output_stream;       // Program output stream
	char *output_stream_name;  // Name of the output stream file
} Config;

void args_process(Config *config, int argc, char **argv);

#endif
