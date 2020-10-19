#include <stdlib.h>
#include <string.h>

#include "buffer.h"
#include "command.h"
#include "config.h"

#define CMDWIDTH 32

static void string_reverse(char *str);
static int decimal_reverse(int num);
static int decimal_remove_last_digit(int num);

// cmd_process: Read, process and execute a command, true implies exit
int cmd_process(Command *cmd, Buffer *buffer, Config *config)
{
	static char cmd_input[CMDWIDTH+1]; // Storage for command input
	if (fscanf(config->input_stream, "%s", cmd_input) == EOF) {
		return 1;
	}

	// Get and remove the number prefix of cmd to get cmd_line, assigning the
	// leftover string to cmd_temp.
	char *cmd_temp;
	cmd->line = strtol(cmd_input, &cmd_temp, 10);
	if (!cmd->line) {
		if (buffer->line_ptr)
			cmd->line = buffer->line_ptr->number;
		else
			cmd->line = 1;
	}

	// Get and remove the number suffix of cmd_temp to get cmd_count, assigning
	// the leftover string to cmd_id.
	strcat(cmd_temp, "1");
	string_reverse(cmd_temp);
	cmd->count = strtol(cmd_temp, &cmd->id, 10);
	cmd->count = decimal_reverse(cmd->count);
	cmd->count = decimal_remove_last_digit(cmd->count);
	if (!cmd->count)
		cmd->count = 1;

	if (strlen(cmd->id) != 1) {
		fprintf(stderr, "Invalid command\n");
	}
	else {
		switch (*cmd->id) {
		case 'f': {

		} break;
		case 'v': {

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

// string_reverse: Reverse a string in-place
static void string_reverse(char *s)
{
	for (char *e = s+strlen(s)-1; s < e; ++s, --e) {
		char temp = *s;
		*s = *e;
		*e = temp;
	}
}

// decimal_reverse: Reverse a decimal by return
static int decimal_reverse(int num)
{
	int rev = 0;
	while (num) {
		rev = rev*10 + num%10;
		num /= 10;
	}
	return rev;
}

// decimal_remove_last_digit: Remove the last digit of a decimal by return
static int decimal_remove_last_digit(int num)
{
	return num / 10;
}
