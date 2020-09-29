#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULTLINEWIDTH    80
#define DEFAULTBUFFERLENGTH 100

int args_process(int argc, char **argv);
int buffer_setup(char **argv);
int buffer_clean(void);

// configs: Program configuration
struct {
	FILE *input_stream;                 // Program input stream
	unsigned int buffer_length;         // Initial buffer length
	unsigned int line_width;            // Maximum line width
	unsigned int buffer_length_set : 1; // Has initial buffer length been set
	unsigned int line_width_set : 1;    // Has maximum line width been set
	unsigned int input_stream_set : 1;  // Has input stream been set
} configs;

// buffer: Text buffer
struct {
	unsigned int length;       // Number of lines stored in the buffer
	unsigned int current_line; // Current line in line buffer
	char **line_ptr;           // Pointer to line buffer
} buffer;

// modes: Program modes
enum modes {
	CMD, // Execute commands
	TXT  // Manipulate text buffer
};

// commands: Commands to be executed in CMD mode
enum commands {
	READ, // Load file contents
	SETL, // Set the current line
	NSRT, // Insert text at the start of the current line
	APND, // Append text to the end of the current line
	WRIT, // Write the buffer to a file
	EXIT  // Exit the program
};

int main(int argc, char **argv)
{
	configs.buffer_length = DEFAULTBUFFERLENGTH;
	configs.line_width = DEFAULTLINEWIDTH;
	configs.input_stream = stdin;

	args_process(argc, argv);
	buffer_setup(argv);

	int mode = CMD;               // Current program mode
	char cmd[DEFAULTLINEWIDTH+1]; // Storage for command input

	while (fscanf(configs.input_stream, "%s", cmd) != EOF) {
		switch (mode) {
			case CMD: {

			} break;
			case TXT: {

			} break;
		}
	}

	buffer_clean();
	return 0;
}

// args_process: Process the command line arguments
int args_process(int argc, char **argv)
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
		else if (!strcmp(*arg_ptr, "--bl") || !strcmp(*arg_ptr, "--buffer-length")) {
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
		else if (!strcmp(*arg_ptr, "--lw") || !strcmp(*arg_ptr, "--line-width")) {
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

	free(error_str);
	return 0;
}

// buffer_setup: Setup buffer members and allocate line memory
int buffer_setup(char **argv)
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

// buffer_clean: Free buffer memory
int buffer_clean(void)
{
	for (int i = 0; i < buffer.length; ++i) {
		free(*(buffer.line_ptr+i));
	}
	free(buffer.line_ptr);

	return 0;
}
