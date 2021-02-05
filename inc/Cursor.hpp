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
    std::array<int, 2> pos;

public:
    Cursor(int x_pos, int y_pos);
};

#endif // CURSOR_HPP_
