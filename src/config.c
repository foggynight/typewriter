#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "error.h"

void args_process(Config *config, int argc, char **argv)
{
    for (char **arg_ptr = argv+1; argc > 1; --argc, ++arg_ptr) {
        // Input stream: [--is|--input-stream] FILENAME
        if (!strcmp(*arg_ptr, "--is") || !strcmp(*arg_ptr, "--input-stream")) {
            if (config->input_stream_set)
                fatal_error("Invalid use: Input stream already set", 1);

            --argc, ++arg_ptr;
            config->input_stream = fopen(*arg_ptr, "r");

            if (!config->input_stream)
                fatal_error("Error: Cannot open input file", 1);
            else
                config->input_stream_set = 1;
        }
        // Output stream: FILENAME
        else {
            if (config->output_stream_name)
                fatal_error("Invalid use: Output stream already set", 1);
            else
                config->output_stream_name = *arg_ptr;
        }
    }
}
