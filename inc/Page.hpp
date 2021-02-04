/**
 * led - Page.hpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef PAGE_HPP_
#define PAGE_HPP_

#include <fstream>
#include <string>
#include <vector>

/* Page type representing a file */
class Page {
public:
    std::vector<std::string> lines;

private:
    std::string path;
    std::fstream file;

public:
    void load(std::string path);
};

#endif // PAGE_HPP_
