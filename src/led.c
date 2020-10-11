/*
 * * led - Line EDitor *
 *
 * ** COMMANDS **
 * Format: [LINE]COMMAND[COUNT]
 *
 * LINE: Target line
 * COMMAND: Command to execute
 * COUNT: Number of times to execute
 *
 * By default, a command is executed 1 time on the current line.
 *
 * Including a target line sets the current line to that target before
 * executing a command. Commands that act on a line will increment the
 * line number after being executed.
 *
 * Commands that modify the line buffer may be repeated using count.
 * Repeating a command will cause the line number to be incremented
 * between executions.
 *
 * ** COMMAND LIST **
 * f - file: Open or create a file
 * v - view: Print the whole line buffer
 * r - read: Print the current line
 * s - setline: Set the current line
 * l - line: Print the current line number
 * i - insert: Insert text before the current line
 * a - append: Append text after the current line
 * c - change: Replace text at the given line
 * w - write: Write line buffer to the current file
 * q - exit: Exit the program
 *
 * Author: foggynight
 */

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CMDWIDTH 32

typedef unsigned int uint;

// Config: Program configuration
struct Config {
	char *program_name;         // Program name
	FILE *input_stream;         // Program input stream
	FILE *output_stream;        // Program output stream
	uint input_stream_set : 1;  // Has input stream been set
	uint output_stream_set : 1; // Has output stream been set
} config;

// Line: A line of text
struct Line {
	uint number;       // Line number
	char *text;        // Text content
	struct Line *prev; // Previous line
	struct Line *next; // Next line
};

// Buffer: Line buffer
struct Buffer {
	struct Line *first_line; // Pointer to first line
	struct Line *last_line;  // Point to last line
	struct Line *line_ptr;   // Pointer to current line
} buffer;

// Command: User command storage
struct Command {
	uint line;  // Target line number
	char *id;   // Command ID
	uint count; // Repeat count times
} cmd;

void args_process(int argc, char **argv);
int cmd_process(void);
void file_load(char *file_name);
void buffer_load(void);
void buffer_clean(void);
void string_reverse(char *str);
int decimal_reverse(int num);
int decimal_remove_last_digit(int num);
void fatal_error(char *str, int code);

int main(int argc, char **argv)
{
	config.program_name = *argv;
	config.input_stream = stdin;
	config.output_stream = stdout;

	args_process(argc, argv);

	int exit = 0;
	while (!exit) {
		exit = cmd_process();
	}

	buffer_clean();
	return 0;
}

// args_process: Process the command line arguments
void args_process(int argc, char **argv)
{
	for (char **arg_ptr = argv+1; argc > 1; --argc, ++arg_ptr) {
		// Input stream: [--is|--input-stream] FILENAME
		if (!strcmp(*arg_ptr, "--is") || !strcmp(*arg_ptr, "--input-stream")) {
			if (config.input_stream_set)
				fatal_error("invalid use: input stream already set", 1);

			--argc, ++arg_ptr;
			config.input_stream = fopen(*arg_ptr, "r");
			if (!config.input_stream)
				fatal_error("memory error", 1);

			config.input_stream_set = 1;
		}
		// Output stream: FILENAME
		else {
			if (config.output_stream_set)
				fatal_error("invalid use: output stream already set", 1);

			file_load(*arg_ptr);
			config.output_stream_set = 1;
		}
	}
}

// cmd_process: Read, process and execute a command, true implies exit
int cmd_process(void)
{
	static char cmd_input[CMDWIDTH+1]; // Storage for command input
	if (fscanf(config.input_stream, "%s", cmd_input) == EOF) {
		return 1;
	}

	// Get and remove the number prefix of cmd to get cmd_line, assigning the
	// leftover string to cmd_temp.
	char *cmd_temp;
	cmd.line = strtol(cmd_input, &cmd_temp, 10);
	if (!cmd.line) {
		if (buffer.line_ptr)
			cmd.line = buffer.line_ptr->number;
		else
			cmd.line = 1;
	}

	// Get and remove the number suffix of cmd_temp to get cmd_count, assigning
	// the leftover string to cmd_id.
	strcat(cmd_temp, "1");
	string_reverse(cmd_temp);
	cmd.count = strtol(cmd_temp, &cmd.id, 10);
	cmd.count = decimal_reverse(cmd.count);
	cmd.count = decimal_remove_last_digit(cmd.count);
	if (!cmd.count)
		cmd.count = 1;

	if (strlen(cmd.id) != 1) {
		fprintf(stderr, "Invalid command\n");
	}
	else {
		switch (*cmd.id) {
		case 'f': {
			char file_name[128];
			printf("Enter filename: ");
			scanf("%128s", file_name);
			getchar();

			file_load(file_name);
		} break;
		case 'v': {
			for (
				struct Line *line_ptr = buffer.first_line;
				line_ptr;
				line_ptr = line_ptr->next
			) {
				printf("%d: %s", line_ptr->number, line_ptr->text);
			}
		} break;
		case 'r': {

		} break;
		case 'l': {

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
			printf("Exiting program\n");
			return 1;
		} break;
		default: {
			fprintf(stderr, "Invalid command\n");
		} break;
		}
	}

	return 0;
}

void file_load(char *file_name)
{
	if (config.output_stream != stdout) {
		buffer_clean();
	}

	config.output_stream = fopen(file_name, "r+");
	if (config.output_stream) {
		printf("Editing file: %s\n", file_name);
	}
	else {
		printf("Creating file: %s\n", file_name);
		if (!(config.output_stream = fopen(file_name, "w+")))
			fatal_error("memory error", 1);
	}

	buffer_load();
}

// buffer_load: Load a file into the buffer
void buffer_load(void)
{
	struct Line *line = malloc(sizeof(struct Line));
	struct Line *prev_line = NULL;
	buffer.first_line = line;

	for (
		size_t count = 0, length;
		getline(&line->text, &length, config.output_stream) != EOF;
		++count
	) {
		line->number = count + 1;

		if (prev_line) {
			line->prev = prev_line;
			prev_line->next = line;
		}
		else {
			line->prev = NULL;
		}

		prev_line = line;
		line = malloc(sizeof(struct Line));
	}

	if (prev_line) {
		prev_line->next = NULL;
		buffer.last_line = prev_line;
		buffer.line_ptr = buffer.first_line;
	}
	else {
		buffer.first_line = buffer.last_line = buffer.line_ptr = NULL;
	}
}

// buffer_clean: Free buffer memory
void buffer_clean(void)
{
	for (
		struct Line *line = buffer.first_line, *next = NULL;
		line;
		line = next
	) {
		next = line->next;
		free(line);
	}

	if (config.output_stream != stdout) {
		fclose(config.output_stream);
	}
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

// decimal_remove_last_digit: Remove the last digit of a decimal by return
int decimal_remove_last_digit(int num)
{
	return num / 10;
}

// fatal_error: Print error message and exit program with an error code
void fatal_error(char *str, int code)
{
	fprintf(stderr, "%s: %s\n", config.program_name, str);
	exit(code);
}
