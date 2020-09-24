#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULTLINEWIDTH    80
#define DEFAULTBUFFERLENGTH 100

struct {
	FILE *input_stream;                 // Program input stream
	unsigned int buffer_length;         // Initial buffer length
	unsigned int line_width;            // Maximum line width
	unsigned int buffer_length_set : 1; // Has initial buffer length been set
	unsigned int line_width_set : 1;    // Has maximum line width been set
	unsigned int input_stream_set : 1;  // Has input stream been set
} configs;

struct {
	unsigned int length;       // Number of lines stored in the buffer
	unsigned int current_line; // Current line in line buffer
	char **line_ptr;           // Pointer to line buffer
} buffer;

int get_args(int argc, char **argv);
int setup_buffer(char **argv);
int clean_buffer(void);

int main(int argc, char **argv)
{
	// Configuration defaults
	configs.buffer_length = DEFAULTBUFFERLENGTH;
	configs.line_width = DEFAULTLINEWIDTH;
	configs.input_stream = stdin;

	// Program setup
	get_args(argc, argv);
	setup_buffer(argv);

	// Storage for user input
	size_t cmd_size;
	char *cmd = malloc(buffer.length * sizeof(char));
	if (!cmd) {
		printf("%s: memory error\n", *argv);
		exit(1);
	}

	// Process user input
	while (getline(&cmd, &cmd_size, configs.input_stream) >= 0) {

	}

	// Program cleanup
	free(cmd);
	clean_buffer();
	return 0;
}

// get_args: Process the command line arguments
int get_args(int argc, char **argv)
{
	char *error_str = malloc(configs.line_width);
	if (!error_str) {
		printf("%s: memory error\n", *argv);
		exit(1);
	}

	for (char **arg_ptr = argv+1; argc > 1; --argc, ++arg_ptr) {
		// Input stream: [--is|--input-stream]
		if (!strcmp(*arg_ptr, "--is") || !strcmp(*arg_ptr, "--input-stream")) {
			if (configs.input_stream_set) {
				printf("%s: invalid use: input stream already set\n", *argv);
				exit(1);
			}
			--argc, ++arg_ptr;
			configs.input_stream = fopen(*arg_ptr, "r");
			if (!configs.input_stream) {
				printf("%s: memory error\n", *argv);
				exit(1);
			}
			else {
				configs.input_stream_set = 1;
			}
		}
		// Buffer length: [--bl|--buffer-length]
		if (!strcmp(*arg_ptr, "--bl") || !strcmp(*arg_ptr, "--buffer-length")) {
			if (configs.buffer_length_set) {
				printf("%s: invalid use: buffer length already set\n", *argv);
				exit(1);
			}
			--argc, ++arg_ptr;
			int buffer_length = strtol(*arg_ptr, &error_str, 10);
			if (*error_str || buffer_length < 1 || buffer_length > UINT_MAX) {
				printf("%s: invalid buffer length\n", *argv);
				exit(1);
			}
			else {
				configs.buffer_length = buffer_length;
				configs.buffer_length_set = 1;
			}
		}
		// Line width: [--lw|--line-width]
		if (!strcmp(*arg_ptr, "--lw") || !strcmp(*arg_ptr, "--line-width")) {
			if (configs.line_width_set) {
				printf("%s: invalid use: line width already set\n", *argv);
				exit(1);
			}
			--argc, ++arg_ptr;
			int line_width = strtol(*arg_ptr, &error_str, 10);
			if (*error_str || line_width < 1 || line_width > UINT_MAX) {
				printf("%s: invalid line width\n", *argv);
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

// setup_buffer: Setup buffer members and allocate line memory
int setup_buffer(char **argv)
{
	buffer.length = configs.buffer_length;
	buffer.current_line = 0;
	buffer.line_ptr = malloc(buffer.length * sizeof(char*));
	if (!buffer.line_ptr) {
		printf("%s: memory error\n", *argv);
		exit(1);
	}

	for (int i = 0; i < buffer.length; ++i) {
		*(buffer.line_ptr+i) = malloc(configs.line_width * sizeof(char));
		if (!*(buffer.line_ptr+i)) {
			printf("%s: memory error\n", *argv);
			exit(1);
		}
	}

	return 0;
}

// clean_buffer: Free buffer memory
int clean_buffer(void)
{
	for (int i = 0; i < buffer.length; ++i) {
		free(*(buffer.line_ptr+i));
	}
	free(buffer.line_ptr);

	return 0;
}
