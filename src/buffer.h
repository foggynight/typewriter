#ifndef BUFFER_H
#define BUFFER_H

#include "config.h"
#include "line.h"

// Buffer: Line buffer
typedef struct buffer {
	Line *first_line; // Pointer to first line
	Line *last_line;  // Point to last line
	Line *line_ptr;   // Pointer to current line
} Buffer;

void buffer_load(Buffer *buffer, Config *config);
void buffer_clean(Buffer *buffer, Config *config);

#endif
