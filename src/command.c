#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "buffer.h"
#include "command.h"
#include "config.h"

#define stringify(x) #x
#define str(x) stringify(x)

#define CMDLEN 32

/* Command input utility functions */
static void string_reverse(char *str);
static int decimal_reverse(int num);
static int decimal_remove_last_digit(int num);

/* Command implementations */
static void file(Buffer *buffer, Config *config);
static void view(Buffer *buffer);
static void read(Command *cmd, Buffer *buffer);
static void line(Buffer *buffer);
static void setline(Command *cmd, Buffer *buffer, Config *config);
static void insert(void);
static void append(void);
static void change(void);
static void write(void);

int cmd_process(Command *cmd, Buffer *buffer, Config *config)
{
    static char cmd_input[CMDLEN+1]; // Storage for command input

    // No need to print an error message, EOF will typically imply the user is
    // exiting the program.
    if (fscanf(config->input_stream, " %" str(CMDLEN) "s", cmd_input) == EOF) {
        return 1;
    }

    {
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
    }

    if (strlen(cmd->id) != 1) {
        printf("Invalid command\n");
        return 0;
    }

    switch (*cmd->id) {
    case 'f':
        file(buffer, config);
    break;
    case 'v':
        view(buffer);
    break;
    case 'r':
        read(cmd, buffer);
    break;
    case 'l':
        line(buffer);
    break;
    case 's':
        setline(cmd, buffer, config);
    break;
    case 'i':
        insert();
    break;
    case 'a':
        append();
    break;
    case 'c':
        change();
    break;
    case 'w':
        write();
    break;
    case 'q':
        printf("Exiting led\n");
        return 1;
    default:
        printf("Invalid command\n");
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
    int rev;
    for (rev = 0; num; num /= 10) {
        rev = rev*10 + num%10;
    }
    return rev;
}

// decimal_remove_last_digit: Remove the last digit of a decimal by return
static int decimal_remove_last_digit(int num)
{
    return num / 10;
}

static void file(Buffer *buffer, Config *config) {
    char filename[MAXFILENAMESTR];

    printf("Filename: ");
    if (fscanf(config->input_stream, " %" str(MAXFILENAMELEN) "s", filename) == EOF)
        fatal_error("Error: Read input failure", 1);

    if (config->output_stream_name)
        free(config->output_stream_name);
    config->output_stream_name = strdup(filename);

    buffer_load(buffer, config);
}

static void view(Buffer *buffer) {
    for (Line *ptr = buffer->first_line; ptr; ptr = ptr->next)
        printf("%s", ptr->text);
}

static void read(Command *cmd, Buffer *buffer) {
    Line *ptr = buffer->line_ptr;
    for (int i = 0; ptr && i < cmd->count; ++i) {
        printf("%s", ptr->text);
        ptr = ptr->next;
    }
}

static void line(Buffer *buffer) {
    printf("%d\n", buffer->line_ptr->number);
}

static void setline(Command *cmd, Buffer *buffer, Config *config) {
    if (cmd->line < 1) {
        cmd->line = 1;
    }
    else if (cmd->line > buffer->last_line->number) {
        cmd->line = buffer->last_line->number;
    }

    while (cmd->line < buffer->line_ptr->number) {
        buffer->line_ptr = buffer->line_ptr->prev;
    }
    while (cmd->line > buffer->line_ptr->number) {
        buffer->line_ptr = buffer->line_ptr->next;
    }

    printf("%d\n", buffer->line_ptr->number);
}

static void insert(void) {}
static void append(void) {}
static void change(void) {}
static void write(void) {}
