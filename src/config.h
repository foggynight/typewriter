// Copyright (C) 2020 Robert Coffey
// Licensed under the GNU GPLv2

#ifndef CONFIG_H
#define CONFIG_H

#include <stdio.h>

#define MAXFILENAMELEN 4095               // Max length of filename
#define MAXFILENAMESTR (MAXFILENAMELEN+1) // Max filename string length

typedef unsigned int uint;

// Config: Program configuration
typedef struct config {
    char *program_name;        // Program name
    FILE *input_stream;        // Program input stream
    FILE *output_stream;       // Program output stream
    uint input_stream_set : 1; // Has input stream been set
    char *output_stream_name;  // Name of the output stream file
} Config;

// args_process: Process the command line arguments
void args_process(Config *config, int argc, char **argv);

#endif
