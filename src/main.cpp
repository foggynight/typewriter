/**
 * led - main.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the MIT license
 **/

#include <cctype>
#include <csignal>
#include <iostream>

#include "Page.hpp"
#include "nc.hpp"

static void finish(int signal);

int main(int argc, char **argv)
{
    Page page = {};

    signal(SIGINT, finish);

    if (argc > 2) {
        std::cerr << "Usage: led [FILENAME]" << std::endl;
        return 1;
    }
    else if (argc > 1) {
        page.file_read(argv[1]);
    }
    else {
        // TODO: Handle no filename argument
        std::cerr << "Usage: led [FILENAME]" << std::endl;
        return 1;
    }

    nc::init();
    nc::draw(page);

    for (int input;
         nc::input(&input);
         nc::draw(page))
    {
        nc::update(page, input);
    }

    std::cerr << "Error: Input failure" << std::endl;
}

static void finish(int signal)
{
    nc::kill();
}
