# led - Scratch Document

This document contains design and planning information related to *led*,
but is intended to serve as a scratchpad for use while implementing the
program, rather than a document for users.


## Introduction

Typewriter inspired text editor

This project was started with a line editor similar to UNIX ed in mind, but
has changed direction.

I want a typewriter for functional use, but I can't find one. I'm going to
write a program to simulate using one instead.


## Program Details

- Text preview and editing
- Visual text editing model
- No modes such as vi
- Pages to represent text files
- Cursor remains static while page slides across the screen
- IO implemented using ncurses


## Scratch

- Main loop updates on user input, no updates while idle
- Each page represents a single text file
- Page content is loaded from a file, edited within the program, then
  written to the file on the user's commands, no continuous writes
