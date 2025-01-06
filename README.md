# TidyKit

A comprehensive toolkit for Free Pascal that provides an easy-to-use, unified interface for common operations. TidyKit is inspired by modern libraries like C++'s Boost and R's Tidyverse, bringing elegant and chainable operations to Free Pascal.

## Features

- 🗂️FileSystem Operations: Modern FS operations inspired by Node.js fs module
- 📝 String Operations: Modern string handling with chainable methods (inspired by R's stringr)
- 📅 DateTime Operations: Lubridate-inspired date and time manipulation
- ⛓️ Chainable API: Fluent interface design for better readability
- 🎯 FPC 3.2.2 Compatible: No inline var, anonymous functions, or lambda
- 💻 Cross-Platform: Works on Windows, Linux, macOS, and FreeBSD

## Platform Compatibility

TidyKit is designed to be platform-independent and works across:
- Windows (32/64-bit)
- Linux
- macOS
- FreeBSD
- Any platform supported by Free Pascal

All operations automatically handle platform-specific differences:
- File paths (directory separators, drive letters)
- Line endings (CR, LF, CRLF)
- File system permissions
- Date/time handling (timezones, DST)

## Installation

1. Clone this repository
2. Add the `src` directory to your project's unit search path
3. Add `TidyKit` to your uses clause

### Using with FPC/Lazarus

```pascal
program MyProject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  TidyKit;  // Add this line to use TidyKit
```

### Using with MSBuild/Delphi

Add the `src` directory to your project's search path in the project options.

## Quick Start

```pascal
uses
  TidyKit;

// String operations example (stringr-inspired)
var
  Str: IStringKit;
  Words: TStringArray;
begin
  Str := Strings;
  
  // Basic transformations
  WriteLn(Str
    .From('  Hello, World!  ')
    .Trim                     // Remove surrounding whitespace
    .ToLower                  // Convert to lowercase
    .Replace('hello', 'hi')   // Replace text
    .ToString);              // 'hi, world!'
    
  // Advanced transformations
  WriteLn(Str
    .From('hello')
    .Reverse                  // Reverse string
    .Duplicate(2)            // Repeat string
    .ToString);              // 'ollehollo'
    
  // Padding and alignment
  WriteLn(Str
    .From('title')
    .PadCenter(20, '*')      // Center with padding
    .ToString);              // '*******title********'
    
  // Whitespace handling
  WriteLn(Str
    .From('  too   many    spaces  ')
    .CollapseWhitespace      // Convert multiple spaces to single
    .Trim                    // Remove surrounding spaces
    .ToString);              // 'too many spaces'
    
  // Pattern matching and extraction
  if Str.From('The year is 2024').Matches('\d+') then
  begin
    // Extract all numbers from text
    for Match in Str.ExtractAll('\d+') do
      WriteLn(Match);        // Prints: 2024
  end;
  
  // Word operations
  Words := Str
    .From('The quick brown fox')
    .Words;                  // Split into words
  for Word in Words do
    WriteLn(Word);          // Prints each word on new line
    
  // Case-insensitive operations
  if Str
    .From('Hello World')
    .Contains('HELLO', False) then  // Case-insensitive search
    WriteLn('Found!');
end;

// DateTime operations example (Lubridate-inspired)
var
  DateKit: IDateTimeKit;
  CurrentYear: Integer;
  NextMonth: TDateTime;
begin
  // Get current year
  CurrentYear := DateTime.Now.Year;
  
  // Chain date manipulations
  NextMonth := DateTime
    .Now                // Start with current date/time
    .StartOfMonth      // Go to start of month
    .AddMonths(1)      // Add one month
    .Hour(9)           // Set to 9 AM
    .Minute(0)         // Set to 0 minutes
    .ToDateTime;       // Convert to TDateTime
    
  // Date parts as both getters and setters
  DateKit := DateTime
    .Now
    .Year(2024)        // Set year
    .Month(3)          // Set month
    .Day(15)           // Set day
    .Hour(14)          // Set hour
    .Minute(30);       // Set minute
    
  // Easy date comparisons
  if DateKit.IsAfter(Now) then
    WriteLn('Future date');
end;

// FileSystem operations example
var
  FS: IFileKit;
begin
  FS := Files;  // Factory function returns IFileKit
  
  // File operations
  FS.ReadFile('input.txt')
    .AppendText('new content')
    .WriteFile('output.txt');
    
  // Directory operations
  FS.CreateDirectory
    .EnsureDirectory;
    
  // Search operations
  for Result in FS.SearchFiles('*.txt', True) do  // True for recursive
    WriteLn(Result.FullPath);
    
  // File information
  WriteLn('Size: ', FS.Size);
  WriteLn('Last Modified: ', FS.LastWriteTime);
  WriteLn('Permissions: ', FS.Attributes.Permissions);  // Unix-style permissions
end;

## FileSystem Operations

TidyKit provides a modern, chainable interface for filesystem operations:

```pascal
// Basic file operations
Files.ReadFile(Path)         // Read file content
     .WriteFile(Path)        // Write to file
     .AppendFile(Path)      // Append to file
     .DeleteFile            // Delete file
     .CopyTo(Path)         // Copy file
     .MoveTo(Path)         // Move/rename file

// Content operations
     .SetContent(Text)      // Set file content
     .AppendText(Text)      // Append text
     .PrependText(Text)     // Prepend text
     .ReplaceText(Old, New) // Replace in content

// Directory operations
     .CreateDirectory       // Create directory
     .DeleteDirectory      // Delete directory
     .EnsureDirectory     // Ensure parent exists

// Path operations
     .ChangeExtension(Ext) // Change file extension
     .GetFileName         // Get file name
     .GetDirectory       // Get directory path
     .GetExtension      // Get file extension

// Search operations
     .SearchFiles(Pattern)    // Search files
     .SearchFilesIn(Dir)     // Search in directory
     .FindNewestFile(Pattern) // Find newest file
     .FindOldestFile(Pattern) // Find oldest file
     .FindLargestFile(Pattern)// Find largest file
     .FindSmallestFile(Pattern)// Find smallest file

// File information
     .Exists              // Check if exists
     .DirectoryExists    // Check if directory
     .Size              // Get file size
     .CreationTime     // Get creation time
     .LastWriteTime   // Get last write time
     .Attributes     // Get file attributes
```

## String Operations (stringr-inspired)

TidyKit provides a rich set of string manipulation functions inspired by R's stringr package:

```pascal
// Basic transformations
Strings.From(Text)           // Create from string
      .Trim                  // Remove whitespace
      .TrimLeft              // Remove left whitespace
      .TrimRight             // Remove right whitespace
      .ToUpper               // Convert to uppercase
      .ToLower               // Convert to lowercase
      .Capitalize            // Capitalize first letter

// Advanced transformations
      .Reverse               // Reverse string
      .Duplicate(N)          // Repeat N times
      .PadLeft(N, Char)      // Left pad to width N
      .PadRight(N, Char)     // Right pad to width N
      .PadCenter(N, Char)    // Center with padding
      .RemoveWhitespace      // Remove all whitespace
      .CollapseWhitespace    // Normalize whitespace

// Pattern matching and replacement
      .Replace(Old, New)     // Replace all occurrences
      .ReplaceRegEx(Pattern) // Replace with regex
      .Extract(Pattern)      // Get regex matches with positions
      .ExtractAll(Pattern)   // Get all regex matches
      .Matches(Pattern)      // Test if matches pattern

// Substrings and parts
      .SubString(Start, Len) // Get substring
      .Left(N)               // First N characters
      .Right(N)              // Last N characters
      .Words                 // Split into words

// Tests and information
      .Contains(Text)        // Check if contains text
      .StartsWith(Text)      // Check if starts with text
      .EndsWith(Text)        // Check if ends with text
      .IsEmpty               // Check if empty
      .Length                // Get string length
      .CountSubString(Text)  // Count occurrences
```

## DateTime Operations (Lubridate-inspired)

The DateTime operations are inspired by R's Lubridate library, providing an intuitive and chainable interface:

```pascal
// Basic operations
DateTime.Now                     // Current date and time
DateTime.Today                   // Current date at midnight

// Get date parts
DateTime.Now.Year               // Current year
DateTime.Now.Month              // Current month
DateTime.Now.Day                // Current day

// Set date parts
DateTime
  .Now
  .Year(2024)
  .Month(3)
  .Day(15);

// Date manipulations
DateTime
  .Now
  .AddMonths(1)                // Add one month
  .StartOfMonth                // Go to first day of month
  .AddDays(7)                  // Add one week
  .StartOfDay;                 // Set time to midnight

// Date comparisons
DateTime
  .Now
  .IsBefore(SomeDate)         // Compare dates
  .IsSameDay(AnotherDate)     // Check if same day
  .IsSameMonth(AnotherDate);  // Check if same month
```

## Documentation

[Detailed documentation coming soon]

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License 