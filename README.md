# typewriter

## Description

typewriter is a text editor inspired by the classic typewriter experience: the
text slides rather than the cursor, there is no backspace or delete (only
'white-out' using the space character), and character inputs overwrite anything
under the cursor.


## Installation

Using SBCL and Quicklisp:
```sh
sbcl --eval "(progn (ql:quickload :croatoan) (exit))"
sbcl --load typewriter.asd --eval "(asdf:make :typewriter)"
```

**Note:** Quicklisp must be added to the SBCL init file for the above commands
to work correctly, this is done during the Quicklisp installation process.


## Usage

```sh
typewriter FILE
```

### Navigating Text
- Arrow keys move the cursor in their corresponding direction
- Backspace moves the cursor backward
- Tab and backtab move the cursor forward and backward respectively by a number
  of cells equal to the config variable `*slide-width*`

### Editing Text
- Printable characters are written at the cursor position and move the cursor
  forward
- Use the space character to clear a cell

### <CTRL> Commands
- `<C-s>`: Save the text to file
- `<C-q>`: Quit the program


## Dependencies

- SBCL
- Quicklisp
- ncurses
- croatoan


## License

Copyright (C) 2020-2021 Robert Coffey

This is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License version 2 as published by the Free Software
Foundation.

You should have received a copy of the GNU General Public License version 2
along with this software. If not, see
[GPLv2 license](https://www.gnu.org/licenses/gpl-2.0).
