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
