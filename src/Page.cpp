// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include "Page.hpp"

#include <fstream>
#include <string>

void Page::file_read(std::string path)
{
    // TODO: Handle invalid filepaths and check if file exists
    // TODO: Expand tabs to spaces before emplacing lines

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
    ++cursor.y;
    cursor.x = 0;
}

void Page::add_char(int src)
{
    while (cursor.y >= lines.size())
        lines.emplace_back();

    size_t line_size = lines[cursor.y].size();
    if (cursor.x < line_size) {
        lines[cursor.y][cursor.x] = src;
    }
    else {
        int i = cursor.x - line_size;
        while (i--)
            lines[cursor.y] += ' ';
        lines[cursor.y] += src;
    }

    ++cursor.x;
}

void Page::move_cursor(int x, int y)
{
    if (x >= 0 || cursor.x >= (size_t)-x)
        cursor.x += x;

    if (y >= 0 || cursor.y >= (size_t)-y)
        cursor.y += y;
}
