#include <stdio.h>
#include <stdlib.h>

#include "buffer.h"
#include "config.h"
#include "error.h"
#include "line.h"

void buffer_load(Buffer *buffer, Config *config)
{
    if (config->output_stream_name) {
        buffer_clean(buffer, config);
        config->output_stream = fopen(config->output_stream_name, "r+");
        if (config->output_stream) {
            printf("Editing file: %s\n", config->output_stream_name);
        }
        else {
            if ((config->output_stream = fopen(config->output_stream_name, "w+")))
                printf("Creating file: %s\n", config->output_stream_name);
            else
                fatal_error("Memory error", 1);
        }
    }

    Line *line = calloc(1, sizeof(Line));
    Line *prev_line = NULL;
    buffer->first_line = line;

    for (
        size_t count = 0, length = 0;
        getline(&line->text, &length, config->output_stream) != EOF;
        ++count
    ) {
        line->number = count + 1;

        if (prev_line) {
            prev_line->next = line;
            line->prev = prev_line;
        }
        else {
            line->prev = NULL;
        }

        prev_line = line;
        line = calloc(1, sizeof(Line));
        length = 0;
    }

    if (prev_line) {
        prev_line->next = NULL;
        buffer->last_line = prev_line;
        buffer->line_ptr = buffer->first_line;
        free(line);
    }
    else {
        line->number = 1;
        buffer->line_ptr = buffer->last_line = line;
    }
}

void buffer_clean(Buffer *buffer, Config *config)
{
    for (
        Line *current = buffer->first_line, *next = NULL;
        current;
        current = next
    ) {
        next = current->next;
        free(current);
    }

    if (config->output_stream != stdout)
        fclose(config->output_stream);
}
