# led

A Line EDitor inspired by ed.

## Installation

```
git clone https://github.com/foggynight/led
cd led
make
```

## Usage

### Commands

**Format:** `[LINE]COMMAND[COUNT]`

- `LINE`: Target line number
- `COMMAND`: Command to execute
- `COUNT`: Number of times to execute

By default, a command is executed 1 time on the current line.

Including a target line sets the current line to that target before
executing a command. Commands that act on a line will increment the
line number after being executed.

Commands that modify the line buffer may be repeated using count.
Repeating a command will cause the line number to be incremented
between executions.

### Command List

- f - file: Open or create a file
- v - view: Print the whole line buffer
- r - read: Print the current line
- l - line: Print the current line number
- s - setline: Set the current line
- i - insert: Insert text before the current line
- a - append: Append text after the current line
- c - change: Replace text at the given line
- w - write: Write line buffer to the current file
- q - exit: Exit the program
