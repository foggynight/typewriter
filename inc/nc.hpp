/**
 * led - nc.hpp
 *
 * All functions that call to ncurses are declared in the nc namespace.
 *
 * Copyright (C) 2021 Robert Coffey
 * Released under the GPLv2 license
 **/

#ifndef NC_HPP_
#define NC_HPP_

#include "Page.hpp"

/* Functions that call to ncurses */
namespace nc
{

void init();
void kill();
void draw(Page& page);

}

#endif // NC_HPP_
