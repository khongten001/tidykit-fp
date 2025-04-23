# File Analyzer Example for TidyKit.FS

This example demonstrates how to use the TidyKit.FS module for file and directory operations.

## Features

This example shows how to:

1. **Analyze text files**:
   - Count lines, words, and characters
   - Determine file encoding
   - Find most common words

2. **Work with directories**:
   - List files and directories
   - Get directory statistics
   - Traverse directory trees

3. **Access file metadata**:
   - Get file sizes
   - Check file dates
   - Detect file types

## How to Use

Run the program and select an option from the menu:

1. **Analyze Text File**: Shows basic statistics about a text file
2. **Find Common Words**: Lists the most frequently used words in a text file
3. **List Directory Contents**: Shows files and directories in a specified path
4. **Show Directory Statistics**: Provides detailed information about a directory
0. **Exit**: Quits the program

## Code Walkthrough

The example is organized into several key areas:

- `AnalyzeTextFile`: Shows how to read a file and count lines, words and characters
- `ShowCommonWords`: Demonstrates text parsing and word frequency analysis
- `ListDirectoryContents`: Shows directory traversal and file listing
- `ShowDirectoryStatistics`: Demonstrates gathering metadata about files and directories

Each function is commented to explain its purpose and how it uses the TidyKit.FS module.

## TidyKit.FS Features Used

- `ReadTextFile`: Reading file contents
- `GetSize`: Determining file size
- `IsTextFile`: Checking file type
- `GetMimeType`: Getting file MIME type
- `GetFileEncoding`: Determining text file encoding
- `ListFiles`: Getting files in a directory
- `ListDirectories`: Getting directories
- `SearchFiles`: Finding files with patterns
- `GetDirectoryInfo`: Getting directory statistics
- `GetLastWriteTime`: Getting file timestamps

## Concepts Demonstrated

- File path handling
- File reading
- Text parsing
- Directory traversal
- File metadata access
- Error handling
