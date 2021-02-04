/**
 * led - page.hpp
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef PAGE_HPP_
#define PAGE_HPP_

#include <fstream>
#include <string>
#include <vector>

class Page {
private:
    std::string path;
    std::fstream file;
    std::vector<std::string> lines;

public:
    bool load(std::string path);
};

#endif // PAGE_HPP_
