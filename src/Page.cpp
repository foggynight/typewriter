/**
 * led - Page.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include "Page.hpp"

#include <string>

void Page::file_read(std::string path)
{
    // TODO: Handle invalid filepaths
    // TODO: Check if file exists

    this->path = path;
    file.open(path);

    for (std::string line;
         std::getline(file, line);
         lines.emplace_back(line));
}

void Page::add_newline()
{
    ++cursor.pos.y;
    cursor.pos.x = 0;

    if (cursor.pos.y == lines.size())
        lines.emplace_back();
}

void Page::add_char(int src)
{
    if (cursor.pos.x < lines[cursor.pos.y].size())
        lines[cursor.pos.y][cursor.pos.x] = src;
    else
        lines[cursor.pos.y] += src;

    ++cursor.pos.x;
}
