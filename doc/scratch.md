# led - Scratch Document

This document contains design and planning information related to *led*.
But it is intended to serve as a scratchpad for use while implementing
the program, rather than a document for users.

## Program Details

- Text preview and editing
- Visual text editing model
- No modes such as vi
- Pages to represent text files
- Cursor remains static while page slides across the screen
- IO implemented using ncurses

## Update Loop

- Main loop updates on user input, no updates while the user is idle

## Pages

- Each page represents a single text file
- Page content is loaded from a file, edited within the program, then
  written to the file on user commands, no continuous writes

## Scratch
