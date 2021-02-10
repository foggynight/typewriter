/**
 * led - main.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include <cctype>
#include <csignal>
#include <iostream>

#include "Page.hpp"
#include "nc.hpp"

static void finish(int signal);

int main(int argc, char **argv)
{
    Page page;

    signal(SIGINT, finish);

    if (argc > 2)
    {
        std::cout << "led: too many arguments" << std::endl;
        return 1;
    }
    else if (argc > 1)
    {
        page.file_read(argv[1]);
    }

    nc::init();
    nc::draw(page);

    for (int input;
         nc::input(&input);
         nc::draw(page))
    {
        if (input == '\n')
            page.add_newline();
        else if (isprint(input))
            page.add_char(input);
    }
}

static void finish(int signal)
{
    nc::kill();
}
