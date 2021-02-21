/**
 * led - Cursor.hpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef CURSOR_HPP_
#define CURSOR_HPP_

class Cursor
{
public:
    struct {
        size_t x;
        size_t y;
    } pos;
};

#endif // CURSOR_HPP_
