/////////////////////////////////////////////////////////
//                                                     //
//                * led - Line EDitor *                //
//                                                     //
//                    ** MODES **                      //
//   CMD - Command Mode                                //
//   TXT - Text Mode                                   //
//                                                     //
//                   ** COMMANDS **                    //
//   Format: [LINE]COMMAND[COUNT]                      //
//                                                     //
//   LINE: Target line                                 //
//   COMMAND: Command to execute                       //
//   COUNT: Number of times to execute                 //
//                                                     //
//   Including a target line sets the current line     //
//   to that target before executing a command.        //
//                                                     //
//   Repeating a command using count causes the line   //
//   number to be incremented between commands.        //
//                                                     //
//   By default, a command is executed 1 time on the   //
//   current line, and most commands increment the     //
//   current line number.                              //
//                                                     //
//                 ** COMMAND LIST: **                 //
//   f - file: Open or create a file                   //
//   v - view: Print the whole line buffer             //
//   r - read: Print the current line                  //
//   s - setline: Set the current line                 //
//   i - insert: Insert text at the start of a line    //
//   a - append: Append text to the end of a line      //
//   c - change: Replace text at the given line        //
//   w - write: Write the buffer to a file             //
//   q - exit: Exit the program                        //
//                                                     //
//   Author: foggynight                                //
//                                                     //
/////////////////////////////////////////////////////////

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULTLINEWIDTH    80
#define DEFAULTBUFFERLENGTH 100

// configs: Program configuration
struct {
	FILE *input_stream;                 // Program input stream
	FILE *output_stream;                // Program output stream
	unsigned int line_width;            // Initial line width
	unsigned int buffer_length;         // Initial buffer length
	unsigned int input_stream_set : 1;  // Has input stream been set
	unsigned int output_stream_set : 1; // Has output stream been set
	unsigned int line_width_set : 1;    // Has initial line width been set
	unsigned int buffer_length_set : 1; // Has initial buffer length been set
} configs;

// buffer: Line buffer and buffer state
struct {
	size_t length;       // Number of lines allocated in memory
	size_t last_line;    // End of the lines filled with text
	size_t current_line; // Current line index for user commands
	char **line_ptr;     // Pointer to line buffer
} buffer;

// modes: Program modes
enum modes {
	CMD, // Execute commands
	TXT, // Manipulate text buffer
	EXIT // Exit the program
};

int args_process(int argc, char **argv);
void fatal_error(char *str, int code);
void buffer_setup(void);
void buffer_load(void);
void buffer_clean(void);
enum modes cmd_process(void);
void string_reverse(char *str);
int decimal_reverse(int num);
int decimal_remove_last_digit(int num);

char *prog_name; // Name of the program

int main(int argc, char **argv)
{
	prog_name = *argv;
	configs.input_stream = stdin;
	configs.output_stream = stdout;
	configs.line_width = DEFAULTLINEWIDTH;
	configs.buffer_length = DEFAULTBUFFERLENGTH;

	args_process(argc, argv);
	buffer_setup();

	int exit = 0;          // Exit flag
	enum modes mode = CMD; // Current program mode
	while (!exit) switch (mode) {
	case CMD: {
		mode = cmd_process();
	} break;
	case TXT: {

	} break;
	case EXIT: {
		exit = 1;
	} break;
	}

	buffer_clean();
	return 0;
}

// cmd_process: Read, process and execute a command
enum modes cmd_process(void)
{
	static char cmd[DEFAULTLINEWIDTH+1]; // Storage for command input
	int cmd_line;                        // Target line
	char *cmd_id;                        // Command ID string
	int cmd_count;                       // Number of times to execute
	enum modes new_mode = CMD;           // New program mode

	if (fscanf(configs.input_stream, "%s", cmd) == EOF) {
		return EXIT;
	}

	// Get and remove the number prefix of cmd to get cmd_line, assigning the
	// leftover string to cmd_temp.
	char *cmd_temp;
	cmd_line = strtol(cmd, &cmd_temp, 10);
	if (!cmd_line) cmd_line = buffer.current_line;

	// Get and remove the number suffix of cmd_temp to get cmd_count, assigning
	// the leftover string to cmd_id.
	strcat(cmd_temp, "1");
	string_reverse(cmd_temp);
	cmd_count = strtol(cmd_temp, &cmd_id, 10);
	cmd_count = decimal_reverse(cmd_count);
	cmd_count = decimal_remove_last_digit(cmd_count);
	if (!cmd_count) cmd_count = 1;

	if (strlen(cmd_id) != 1) {
		fprintf(stderr, "Invalid command.\n");
	}
	else {
		while (cmd_count--) switch (*cmd_id) {
		case 'f': {
			char file_name[128];
			printf("Enter filename: ");
			scanf("%128s", file_name);
			getchar();

			if (configs.output_stream != stdout) {
				free(configs.output_stream);
			}
			configs.output_stream = fopen(file_name, "r+");

			if (configs.output_stream) {
				printf("Editing file: %s\n", file_name);
			}
			else {
				printf("Creating file: %s\n", file_name);
				configs.output_stream = fopen(file_name, "w+");
			}

			buffer_load();
		} break;
		case 'v': {
			for (size_t i = 0; i < buffer.last_line; ++i) {
				printf("%zu: %s", i+1, *(buffer.line_ptr + i));
			}
		} break;
		case 'r': {
			if (buffer.current_line <= buffer.last_line) {
				printf("%zu: %s",
					buffer.current_line,
					*(buffer.line_ptr + buffer.current_line - 1)
				);
			}
		} break;
		case 's': {

		} break;
		case 'i': {

		} break;
		case 'a': {

		} break;
		case 'c': {

		} break;
		case 'w': {

		} break;
		case 'q': {

		} break;
		default: {
			fprintf(stderr, "Invalid command.\n");
		} break;
		}
	}

	return new_mode;
}

// args_process: Process the command line arguments
int args_process(int argc, char **argv)
{
	char *error_str = malloc(configs.line_width);
	if (!error_str) fatal_error("memory error", 1);

	for (char **arg_ptr = argv+1; argc > 1; --argc, ++arg_ptr) {
		// Buffer length: [--bl|--buffer-length]
		if (!strcmp(*arg_ptr, "--bl") || !strcmp(*arg_ptr, "--buffer-length")) {
			if (configs.buffer_length_set)
				fatal_error("invalid use: buffer length already set", 1);

			--argc, ++arg_ptr;
			int buffer_length = strtol(*arg_ptr, &error_str, 10);
			if (*error_str || buffer_length < 1 || buffer_length > UINT_MAX)
				fatal_error("invalid buffer length", 1);

			configs.buffer_length = buffer_length;
			configs.buffer_length_set = 1;
		}
		// Line width: [--lw|--line-width]
		else if (!strcmp(*arg_ptr, "--lw") || !strcmp(*arg_ptr, "--line-width")) {
			if (configs.line_width_set)
				fatal_error("invalid use: line width already set", 1);

			--argc, ++arg_ptr;
			int line_width = strtol(*arg_ptr, &error_str, 10);
			if (*error_str || line_width < 1 || line_width > UINT_MAX)
				fatal_error("invalid line width", 1);

			configs.line_width = line_width;
			configs.line_width_set = 1;
		}
		// Input stream: [--is|--input-stream]
		else if (!strcmp(*arg_ptr, "--is") || !strcmp(*arg_ptr, "--input-stream")) {
			if (configs.input_stream_set)
				fatal_error("invalid use: input stream already set", 1);

			--argc, ++arg_ptr;
			configs.input_stream = fopen(*arg_ptr, "r");
			if (!configs.input_stream)
				fatal_error("memory error", 1);

			configs.input_stream_set = 1;
		}
		// Output stream: Argument provided without a selector
		else {
			if (configs.output_stream_set)
				fatal_error("invalid use: output stream already set", 1);

			configs.output_stream = fopen(*arg_ptr, "r+");
			if (configs.output_stream) {
				printf("Editing file: %s\n", *arg_ptr);
			}
			else {
				printf("Creating file: %s\n", *arg_ptr);
				configs.output_stream = fopen(*arg_ptr, "w+");
			}

			configs.output_stream_set = 1;
		}
	}

	free(error_str);
	return 0;
}

// buffer_setup: Setup buffer members and allocate line memory
void buffer_setup(void)
{
	buffer.length = configs.buffer_length;
	buffer.line_ptr = malloc(buffer.length * sizeof(char*));
	if (!buffer.line_ptr) fatal_error("memory error", 1);

	// Needs space for the line, a newline character and null
	for (size_t i = 0; i < buffer.length; ++i) {
		*(buffer.line_ptr+i) = calloc(1, configs.line_width + 2);
		if (!*(buffer.line_ptr+i)) fatal_error("memory error", 1);
	}

	if (configs.output_stream != stdout) {
		buffer_load();
	}
}

// buffer_load: Load a file into the buffer
void buffer_load(void)
{
	buffer.current_line = 1;

	size_t count, width;
	for (
		count = 0, width = configs.line_width + 2;
		getline(buffer.line_ptr+count, &width, configs.output_stream) != EOF;
		++count
	);
	buffer.last_line = count;
}

// buffer_clean: Free buffer memory
void buffer_clean(void)
{
	for (int i = 0; i < buffer.length; ++i) {
		free(*(buffer.line_ptr+i));
	}
	free(buffer.line_ptr);
}

// string_reverse: Reverse a string in-place
void string_reverse(char *s)
{
	for (char *e = s+strlen(s)-1; s < e; ++s, --e) {
		char temp = *s;
		*s = *e;
		*e = temp;
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

// fatal_error: Print error message and exit program with an error code
void fatal_error(char *str, int code)
{
	fprintf(stderr, "%s: %s\n", prog_name, str);
	exit(code);
}
