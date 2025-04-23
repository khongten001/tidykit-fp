# Simple File Explorer

A beginner-friendly example of using TidyKit.FS for high school programming students.

## What This Example Shows

This example creates an interactive file explorer program that demonstrates many important programming concepts:

1. **File handling** - Reading, writing, copying and deleting files
2. **Directory navigation** - Moving through folders and listing contents  
3. **User interaction** - Command processing and input/output
4. **Error handling** - Checking for problems and handling them safely

## Features

- **Directory browsing**: Navigate through folders on your computer
- **File viewing**: Read and display text files
- **File creation**: Create new text files
- **File operations**: Copy, move, and delete files
- **File information**: See details about files (size, date, type)

## Learning Objectives

This example will help you understand:

- How computers organize files and folders
- How to work with file paths
- How to read from and write to files
- How to handle user input safely
- How to build an interactive menu system

## Commands

| Command | Description |
|---------|-------------|
| `CD [dir]` | Change to directory (or `..` for parent) |
| `VIEW [file]` | View contents of a text file |
| `INFO [file]` | Show detailed information about a file |
| `COPY [src] [dest]` | Copy a file to a new location |
| `MOVE [src] [dest]` | Move a file to a new location |
| `NEW [file]` | Create a new text file |
| `DEL [file]` | Delete a file |
| `EXIT` | Exit the program |
| `HELP` | Display available commands |

## Key TidyKit.FS Features Used

- `FileKit.ReadTextFile` - Reading text files
- `FileKit.WriteTextFile` - Writing text files
- `FileKit.CopyFile` - Copying files
- `FileKit.MoveFile` - Moving files
- `FileKit.DeleteFile` - Deleting files
- `FileKit.Exists` - Checking if a file exists
- `FileKit.DirectoryExists` - Checking if a directory exists
- `FileKit.GetSize` - Getting file size
- `FileKit.GetLastWriteTime` - Getting last modified date
- `FileKit.ListFiles` - Listing files in a directory
- `FileKit.ListDirectories` - Listing subdirectories
- `FileKit.GetCurrentDir` - Getting current working directory
- `FileKit.GetParentDir` - Getting parent directory
- `FileKit.CombinePaths` - Combining path components safely
- `FileKit.CountLines` - Counting lines in a text file
- `FileKit.IsTextFile` - Checking if a file is text or binary
- `FileKit.GetFileEncoding` - Detecting file encoding

## Project Structure

The main program is organized into sections:

1. **Helper Functions** - Small functions that perform specific tasks
2. **File Operations** - Functions that work with files
3. **Command Processing** - Code that handles user commands
4. **Main Program** - The core program logic

## Programming Concepts Demonstrated

- **Records and Objects** - Using TidyKit's object-oriented design
- **Interfaces** - Working with the IFileKit interface
- **Arrays** - Using TFilePathArray for file listings
- **Exception Handling** - Using try/except for error management
- **String Processing** - Parsing commands and parameters
- **Control Flow** - Using loops and conditionals
- **Parameter Passing** - Passing values to procedures and functions

## Extending the Project

Here are some ideas for extending this example:
- Add a search function to find files by name or content
- Add ability to create and delete directories
- Add a simple text editor for modifying files
- Add file sorting options (by name, date, size)
- Add a graphical interface using Lazarus/LCL
