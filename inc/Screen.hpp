/**
 * led - Screen.hpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef SCREEN_HPP_
#define SCREEN_HPP_

#include "Page.hpp"

class Screen {
public:
    int width;
    int height;

public:
    void init();
    void kill();
    void draw(Page page);
};

#endif // SCREEN_HPP_
