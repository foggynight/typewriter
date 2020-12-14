/* --- led ---
 *
 * Line EDitor inspired by ed.
 *
 * Copyright (C) 2020 Robert Coffey
 * Licensed under the GNU GPLv2 */

#include <stdio.h>

#include "buffer.h"
#include "command.h"
#include "config.h"

int main(int argc, char **argv)
{
    Config config = {*argv, stdin, stdout, 0, NULL};
    Buffer buffer = {0};
    Command cmd = {0};

    args_process(&config, argc, argv);
    buffer_load(&buffer, &config);

    while (!cmd_process(&cmd, &buffer, &config));

    buffer_clean(&buffer, &config);
    return 0;
}
