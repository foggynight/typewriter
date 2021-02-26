// Copyright (C) 2021 Robert Coffey
// Released under the MIT license


#ifndef NC_HPP_
#define NC_HPP_

#include "Page.hpp"

/* All functions that call to ncurses are declared in the nc namespace */
namespace nc
{

void init();
void kill();
void draw(Page& page);
bool input(int *dest);
void update(Page& page, int input);

}

#endif // NC_HPP_
