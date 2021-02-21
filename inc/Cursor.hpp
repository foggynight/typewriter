/**
 * led - Cursor.hpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef CURSOR_HPP_
#define CURSOR_HPP_

#include <cstddef>

class Cursor
{
public:
    size_t x; // x-y position of the cursor, this position is relative
    size_t y; // to the page containing the cursor
};

#endif // CURSOR_HPP_
