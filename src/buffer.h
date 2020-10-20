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

// buffer_load: Open an output stream and load it into the line buffer
void buffer_load(Buffer *buffer, Config *config);

// buffer_clean: Free line buffer memory and close the output stream
void buffer_clean(Buffer *buffer, Config *config);

#endif
