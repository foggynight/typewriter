/////////////////////////////////////////////////////
//                                                 //
//              * led - Line EDitor *              //
//                                                 //
//                  ** MODES **                    //
// CMD - Command Mode                              //
// TXT - Text Mode                                 //
//                                                 //
//                 ** COMMANDS **                  //
// Format: [LINE]COMMAND[COUNT]                    //
//                                                 //
// LINE: Target line                               //
// COMMAND: Command to execute                     //
// COUNT: Number of times to execute the command   //
//                                                 //
// By default, a command is executed 1 time on the //
// current line.                                   //
//                                                 //
//               ** COMMAND LIST: **               //
// v - view: View the entire file.                 //
// r - read: Print a line to screen.               //
// s - setline: Set the current line.              //
// i - insert: Insert text at the start of a line. //
// a - append: Append text to the end of a line.   //
// w - write: Write the buffer to a file.          //
// q - exit: Exit the program.                     //
//                                                 //
// Author: foggynight                              //
/////////////////////////////////////////////////////

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULTLINEWIDTH    80
#define DEFAULTBUFFERLENGTH 100

int args_process(int argc, char **argv);
void fatal_error(char *str, int code);
int buffer_setup(char **argv);
int buffer_clean(void);
enum modes cmd_process(char *cmd);
void string_reverse(char *str);
int decimal_reverse(int num);
int decimal_remove_last_digit(int num);

// configs: Program configuration
struct {
	FILE *input_stream;                 // Program input stream
	FILE *output_stream;                // Program output stream
	unsigned int buffer_length;         // Initial buffer length
	unsigned int line_width;            // Maximum line width
	unsigned int input_stream_set : 1;  // Has input stream been set
	unsigned int output_stream_set : 1; // Has output stream been set
	unsigned int buffer_length_set : 1; // Has initial buffer length been set
	unsigned int line_width_set : 1;    // Has maximum line width been set
} configs;

// buffer: Text buffer
struct {
	unsigned int length;       // Number of lines stored in the buffer
	unsigned int current_line; // Current line buffer index
	char **line_ptr;           // Pointer to line buffer
} buffer;

// modes: Program modes
enum modes {
	CMD, // Execute commands
	TXT  // Manipulate text buffer
};

int main(int argc, char **argv)
{
	configs.buffer_length = DEFAULTBUFFERLENGTH;
	configs.line_width = DEFAULTLINEWIDTH;
	configs.input_stream = stdin;
	configs.output_stream = stdout;

	args_process(argc, argv);
	buffer_setup(argv);

	int mode = CMD;               // Current program mode
	char cmd[DEFAULTLINEWIDTH+1]; // Storage for command input

	int exit = 0;
	while (!exit) switch (mode) {
	case CMD: {
		while (fscanf(configs.input_stream, "%s", cmd) != EOF) {
			mode = cmd_process(cmd);
		}
	} break;
	case TXT: {

	} break;
	}

	buffer_clean();
	return 0;
}

// args_process: Process the command line arguments
int args_process(int argc, char **argv)
{
	char *error_str = malloc(configs.line_width);
	if (!error_str) fatal_error("%s: memory error\n", 1);

	for (char **arg_ptr = argv+1; argc > 1; --argc, ++arg_ptr) {
		// Buffer length: [--bl|--buffer-length]
		if (!strcmp(*arg_ptr, "--bl") || !strcmp(*arg_ptr, "--buffer-length")) {
			if (configs.buffer_length_set)
				fatal_error("%s: invalid use: buffer length already set\n", 1);

			--argc, ++arg_ptr;
			int buffer_length = strtol(*arg_ptr, &error_str, 10);
			if (*error_str || buffer_length < 1 || buffer_length > UINT_MAX)
				fatal_error("%s: invalid buffer length\n", 1);

			configs.buffer_length = buffer_length;
			configs.buffer_length_set = 1;
		}
		// Line width: [--lw|--line-width]
		else if (!strcmp(*arg_ptr, "--lw") || !strcmp(*arg_ptr, "--line-width")) {
			if (configs.line_width_set)
				fatal_error("%s: invalid use: line width already set\n", 1);

			--argc, ++arg_ptr;
			int line_width = strtol(*arg_ptr, &error_str, 10);
			if (*error_str || line_width < 1 || line_width > UINT_MAX)
				fatal_error("%s: invalid line width\n", 1);

			configs.line_width = line_width;
			configs.line_width_set = 1;
		}
		// Input stream: [--is|--input-stream]
		else if (!strcmp(*arg_ptr, "--is") || !strcmp(*arg_ptr, "--input-stream")) {
			if (configs.input_stream_set)
				fatal_error("%s: invalid use: input stream already set\n", 1);

			--argc, ++arg_ptr;
			configs.input_stream = fopen(*arg_ptr, "r");
			if (!configs.input_stream)
				fatal_error("%s: memory error\n", 1);

			configs.input_stream_set = 1;
		}
		// Output stream: Argument provided without a selector
		else {
			if (configs.output_stream_set)
				fatal_error("%s: invalid use: output stream already set\n", 1);

			configs.output_stream = fopen(*argv, "w");
			if (!configs.output_stream)
				fatal_error("%s: memory error\n", 1);

			configs.output_stream_set = 1;
		}
	}

	free(error_str);
	return 0;
}

// buffer_setup: Setup buffer members and allocate line memory
int buffer_setup(char **argv)
{
	buffer.length = configs.buffer_length;
	buffer.current_line = 1;
	buffer.line_ptr = malloc(buffer.length * sizeof(char*));
	if (!buffer.line_ptr) fatal_error("%s: memory error\n", 1);

	for (int i = 0; i < buffer.length; ++i) {
		*(buffer.line_ptr+i) = malloc(configs.line_width+1);
		if (!*(buffer.line_ptr+i)) fatal_error("%s: memory error\n", 1);
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

// cmd_process: Process and execute a command
enum modes cmd_process(char *cmd)
{
	int cmd_line;              // Target line
	char *cmd_id;              // Command ID string
	int cmd_count;             // Number of times to execute
	enum modes new_mode = CMD; // New program mode
												   
	// Get and remove the number prefix of cmd to get cmd_line, assigning the
	// leftover string to cmd_temp.
	char *cmd_temp;
	if (*cmd != '.') {
		cmd_line = strtol(cmd, &cmd_temp, 10);
		if (!cmd_line) cmd_line = buffer.current_line;
	}
	else {
		cmd_line = buffer.current_line;
		++cmd;
	}

	// Reverse cmd_temp, get and remove the number prefix of cmd_temp, then
	// reverse that number to get the cmd_count, assinging the leftover string
	// to cmd_id.
	strcat(cmd_temp, "1");
	string_reverse(cmd_temp);
	cmd_count = strtol(cmd_temp, &cmd_id, 10);
	cmd_count = decimal_reverse(cmd_count);
	cmd_count = decimal_remove_last_digit(cmd_count);
	if (!cmd_count) cmd_count = 1;

	if (strlen(cmd_id) != 1) {
		printf("Invalid command.\n");
	}
	else {
		switch (*cmd_id) {
		case 'v': {

		} break;
		case 'r': {

		} break;
		case 's': {

		} break;
		case 'i': {

		} break;
		case 'a': {

		} break;
		case 'w': {

		} break;
		case 'q': {

		} break;
		default: {

		} break;
		}
	}
	return new_mode;
}

// fatal_error: Print error message and exit program with an error code
void fatal_error(char *str, int code)
{
	printf("%s\n", str);
	exit(code);
}

// string_reverse: Reverse a string in-place
void string_reverse(char *str)
{
	char *end = str + strlen(str) - 1;
	while (str < end) {
		char temp;
		temp = *str;
		*str = *end;
		*end = temp;
		++str, --end;
	}
}

// decimal_remove_last_digit: Remove the last digit of a decimal by return
int decimal_remove_last_digit(int num)
{
	return num / 10;
}

// decimal_reverse: Reverse a decimal by return
int decimal_reverse(int num)
{
	int rev = 0;
	while (num) {
		rev = rev*10 + num%10;
		num /= 10;
	}
	return rev;
}
