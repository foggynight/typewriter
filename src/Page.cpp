/**
 * led - Page.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include "Page.hpp"

#include <fstream>
#include <string>

void Page::file_read(std::string path)
{
    // TODO: Handle invalid filepaths
    // TODO: Check if file exists

    this->path = path;
    file.open(path, std::fstream::in);

    for (std::string line;
         std::getline(file, line);
         lines.emplace_back(line));

    if (lines.size() == 0)
        lines.emplace_back();

    file.close();
}

void Page::file_write()
{
    file.open(path, std::fstream::out);

    for (auto line : lines)
        file << line << '\n';

    file.close();
}

void Page::add_newline()
{
    ++cursor.pos.y;
    cursor.pos.x = 0;
}

void Page::add_char(int src)
{
    while (cursor.pos.y >= lines.size())
        lines.emplace_back();

    size_t line_size = lines[cursor.pos.y].size();
    if (cursor.pos.x < line_size)
        lines[cursor.pos.y][cursor.pos.x] = src;
    else {
        int i = cursor.pos.x - line_size;
        while (i--)
            lines[cursor.pos.y] += ' ';
        lines[cursor.pos.y] += src;
    }

    ++cursor.pos.x;
}

void Page::move_cursor(int x, int y)
{
    if (x < 0) {
        if (cursor.pos.x >= (size_t)-x)
            cursor.pos.x += x;
    }
    else {
        cursor.pos.x += x;
    }

    if (y < 0) {
        if (cursor.pos.y >= (size_t)-y)
            cursor.pos.y += y;
    }
    else {
        cursor.pos.y += y;
    }
}
