typedef unsigned int uint;

// Line: A line of text
typedef struct line {
	uint number;       // Line number
	char *text;        // Text content
	struct line *prev; // Previous line
	struct line *next; // Next line
} Line;
