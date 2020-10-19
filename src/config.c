#include <string.h>

#include "config.h"
#include "error.h"

// args_process: Process the command line arguments
void args_process(Config *config, int argc, char **argv)
{
	for (char **arg_ptr = argv+1; argc > 1; --argc, ++arg_ptr) {
		// Input stream: [--is|--input-stream] FILENAME
		if (!strcmp(*arg_ptr, "--is") || !strcmp(*arg_ptr, "--input-stream")) {
			if (config->input_stream_set)
				fatal_error("invalid use: input stream already set", 1);

			--argc, ++arg_ptr;
			config->input_stream = fopen(*arg_ptr, "r");
			if (!config->input_stream)
				fatal_error("memory error", 1);

			config->input_stream_set = 1;
		}
		// Output stream: FILENAME
		else {
			if (config->output_stream_name)
				fatal_error("invalid use: output stream already set", 1);

			config->output_stream_name = *arg_ptr;
		}
	}
}
