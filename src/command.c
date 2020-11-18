// SPDX-License-Identifier: GPL-2.0
// Copyright (C) 2020 Robert Coffey

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "buffer.h"
#include "command.h"
#include "config.h"

/**
 * Convert x to a string constant.
 *
 * @params
 * - x {symbol}: Symbols to convert to a string constant
 *
 * @return {const char *}: String constant of x
 *
 * @example
 * const char *str = str(symbols to convert)
 * str -> "symbols to convert"
 *
 * @example
 * #define MAX 32
 * "%" str(MAX) "s" -> "%32s"
 */
#define stringify(x) #x
#define str(x) stringify(x)

#define CMDLEN 32 // Max length of command input

/* Command utility functions */
static void string_reverse(char *str);
static int decimal_reverse(int num);
static int decimal_remove_last_digit(int num);
static int cmd_execute(Command *cmd, Buffer *buffer, Config *config);

/* File operation commands */
static void file(Buffer *buffer, Config *config);
static void write(Buffer *buffer, Config *config);
static void quit(void);

/* Read buffer commands */
static void read(Command *cmd, Buffer *buffer);
static void view(Buffer *buffer);

/* Cursor commands */
static void line(Buffer *buffer);
static void setline(Command *cmd, Buffer *buffer, Config *config);

/* Buffer manipulation commands */
static Line *get_text(Command *cmd, Buffer *buffer, Config *config);
static void append(Command *cmd, Buffer *buffer, Config *config);
static void change(Command *cmd, Buffer *buffer, Config *config);
static void insert(Command *cmd, Buffer *buffer, Config *config);

int cmd_process(Command *cmd, Buffer *buffer, Config *config)
{
    static char cmd_input[CMDLEN+1]; // Storage for command input

    // No need to print an error message, EOF will typically imply the user is
    // exiting the program.
    if (fscanf(config->input_stream, " %" str(CMDLEN) "s", cmd_input) == EOF) {
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
    if (!cmd->count) cmd->count = 1;

    if (strlen(cmd->id) != 1) {
        puts("Invalid command");
        return 0;
    }

    return cmd_execute(cmd, buffer, config);
}

static int cmd_execute(Command *cmd, Buffer *buffer, Config *config) {
    switch (*cmd->id) {
        case 'f': file(buffer, config); break;
        case 'v': view(buffer); break;
        case 'r': read(cmd, buffer); break;
        case 'l': line(buffer); break;
        case 's': setline(cmd, buffer, config); break;
        case 'i': insert(cmd, buffer, config); break;
        case 'a': append(cmd, buffer, config); break;
        case 'c': change(cmd, buffer, config); break;
        case 'w': write(buffer, config); break;
        case 'q':
            quit();
            return 1;
        default: puts("Invalid command"); break;
    }
    return 0;
}

/* --- Command utility functions --- */

/**
 * Reverse a string in-place.
 *
 * @params
 * - s {char*}: String to reverse
 */
static void string_reverse(char *s)
{
    for (char *e = s+strlen(s)-1; s < e; ++s, --e) {
        char temp = *s;
        *s = *e;
        *e = temp;
    }
}

/**
 * Reverse a decimal integer by return.
 *
 * @params
 * - num {int}: Decimal integer to reverse
 *
 * @return {int}: Reversed decimal integer
 */
static int decimal_reverse(int num)
{
    int rev;
    for (rev = 0; num; num /= 10)
        rev = rev*10 + num%10;
    return rev;
}

/**
 * Remove the last digit of a decimal by return.
 *
 * @params
 * - num {int}: Number to dismember
 *
 * @return {int}: Dismembered number
 */
static int decimal_remove_last_digit(int num)
{
    return num / 10;
}

/* --- File operation commands --- */

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

static void write(Buffer *buffer, Config *config)
{
    config->output_stream = fopen(config->output_stream_name, "w");
    if (!config->output_stream)
        fatal_error("Error: Write output failure", 1);

    for (Line *ptr = buffer->first_line; ptr; ptr = ptr->next)
        fprintf(config->output_stream, "%s", ptr->text);
    fclose(config->output_stream);

    config->output_stream = fopen(config->output_stream_name, "r+");
    if (!config->output_stream)
        fatal_error("Error: Write output failure", 1);
}

static void quit(void)
{
    puts("Exiting led...");
}

/* --- Read buffer commands --- */

static void read(Command *cmd, Buffer *buffer) {
    Line *ptr = buffer->line_ptr;
    for (int i = 0; ptr && i < cmd->count; ++i) {
        printf("%s", ptr->text);
        ptr = ptr->next;
    }
}

static void view(Buffer *buffer) {
    for (Line *ptr = buffer->first_line; ptr; ptr = ptr->next)
        printf("%s", ptr->text);
}

/* --- Cursor commands --- */

static void line(Buffer *buffer) {
    printf("%d\n", buffer->line_ptr->number);
}

static void setline(Command *cmd, Buffer *buffer, Config *config) {
    if (cmd->line < 1)
        cmd->line = 1;
    else if (cmd->line > buffer->last_line->number)
        cmd->line = buffer->last_line->number;

    while (cmd->line < buffer->line_ptr->number)
        buffer->line_ptr = buffer->line_ptr->prev;
    while (cmd->line > buffer->line_ptr->number)
        buffer->line_ptr = buffer->line_ptr->next;

    printf("%d\n", buffer->line_ptr->number);
}

/* --- Buffer manipulation commands --- */

static Line *get_text(Command *cmd, Buffer *buffer, Config *config) {}
static void append(Command *cmd, Buffer *buffer, Config *config) {}
static void change(Command *cmd, Buffer *buffer, Config *config) {}
static void insert(Command *cmd, Buffer *buffer, Config *config) {}
