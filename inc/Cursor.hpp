/**
 * led - Cursor.hpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef CURSOR_HPP_
#define CURSOR_HPP_

#include <array>

class Cursor
{
public:
    struct {
        int x;
        int y;
    } pos;
};

#endif // CURSOR_HPP_
