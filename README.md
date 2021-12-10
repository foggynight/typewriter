typewriter
==========

Typewriter inspired text editor at the command line, written in Common Lisp
using the croatoan library.

This is a text editor which behaves like a typewriter: the text slides rather
than the cursor, character inputs overwrite anything under the cursor, and there
is no backspace or delete, only 'white-out' using the space character.


Installation
------------

To build `typewriter` using SBCL:

    sbcl --eval "(progn (ql:quickload :croatoan) (exit))"
    sbcl --load typewriter.asd --eval "(asdf:make :typewriter)"

This will create the `typewriter` executable.


Usage
-----

To create or edit a file:

    ./typewriter FILE

### Navigating Text

- Arrow keys move the cursor in their corresponding direction.
- Backspace moves the cursor backward.
- Tab and backtab move the cursor forward and backward respectively by a number
  of cells equal to the config variable `*slide-width*`.

### Editing Text

- Printable characters are written at the cursor position and move the cursor
  forward.
- Use the space character to clear a cell.

### `<CTRL>` Commands

- `<C-s>`: Save the text to FILE.
- `<C-q>`: Exit `typewriter`.


Dependencies
------------

- SBCL
- Quicklisp
- ncurses


License
-------

Copyright (C) 2020-2021 Robert Coffey

This is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License version 3.
