/**
 * led - main.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include <csignal>
#include <iostream>

#include "Page.hpp"
#include "Screen.hpp"

Screen screen;

int main(int argc, char **argv)
{
    Page page;

    if (argc > 2) {
        std::cout << "led: too many arguments" << std::endl;
        return 1;
    }
    else if (argc > 1) {
        page.load(argv[1]);
    }

    screen.init();
    while (true)
        screen.draw(page);
}

static void finish(int signal)
{
    screen.kill();
}
