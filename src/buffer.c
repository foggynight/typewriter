#include <stdio.h>
#include <stdlib.h>

#include "buffer.h"
#include "config.h"
#include "error.h"
#include "line.h"

// buffer_load: Load a file into the buffer
void buffer_load(Buffer *buffer, Config *config)
{
	if (config->output_stream != stdout) {
		buffer_clean(buffer, config);
	}

	config->output_stream = fopen(config->output_stream_name, "r+");
	if (config->output_stream) {
		printf("Editing file: %s\n", config->output_stream_name);
	}
	else {
		printf("Creating file: %s\n", config->output_stream_name);
		if (!(config->output_stream = fopen(config->output_stream_name, "w+")))
			fatal_error("memory error", 1);
	}

	Line *line = malloc(sizeof(Line));
	Line *prev_line = NULL;
	buffer->first_line = line;

	for (
		size_t count = 0, length;
		getline(&line->text, &length, config->output_stream) != EOF;
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
		line = malloc(sizeof(Line));
	}

	if (prev_line) {
		prev_line->next = NULL;
		buffer->last_line = prev_line;
		buffer->line_ptr = buffer->first_line;
	}
	else {
		buffer->first_line = buffer->last_line = buffer->line_ptr = NULL;
	}
}

// buffer_clean: Free buffer memory
void buffer_clean(Buffer *buffer, Config *config)
{
	for (
		Line *line = buffer->first_line, *next = NULL;
		line;
		line = next
	) {
		next = line->next;
		free(line);
	}

	if (config->output_stream != stdout) {
		fclose(config->output_stream);
	}
}
