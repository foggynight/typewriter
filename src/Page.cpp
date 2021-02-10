/**
 * led - Page.cpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#include "Page.hpp"

#include <string>

void Page::load(std::string path)
{
    // TODO: Handle invalid filepaths
    // TODO: Check if file exists

    this->path = path;
    file.open(path);

    for (std::string line;
         std::getline(file, line);
         lines.push_back(std::string(line)));
}

void Page::newline()
{
    ++cursor.pos.y;
    cursor.pos.x = 0;
}

void Page::write(int src)
{
    if (cursor.pos.x < lines[cursor.pos.y].size())
        lines[cursor.pos.y][cursor.pos.x] = src;
    else
        lines[cursor.pos.y] += src;

    ++cursor.pos.x;
}
