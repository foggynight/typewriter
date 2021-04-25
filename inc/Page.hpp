// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef PAGE_HPP_
#define PAGE_HPP_

#include <fstream>
#include <string>
#include <vector>

struct Cursor
{
    size_t x; // x-y position of the cursor, this position is relative
    size_t y; // to the page containing the cursor
};

/* Page type representing a file */
class Page
{
public:
    Cursor cursor;
    std::vector<std::string> lines;

private:
    std::string path;
    std::fstream file;

public:
    void file_read(std::string path);
    void file_write();

    void add_newline();
    void add_char(int src);

    void move_cursor(int x, int y);
};

#endif // PAGE_HPP_
